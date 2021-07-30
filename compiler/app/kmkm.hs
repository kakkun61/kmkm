{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import Language.Kmkm (Exception (CompileDotDotPathException, CompileModuleNameMismatchException, CompileRecursionException, IntermediateCEmbeddedParameterMismatchException, NameResolveUnknownIdentifierException, ParseException, TypeCheckBindProcedureEndException, TypeCheckMismatchException, TypeCheckNotFoundException, TypeCheckPrimitiveTypeException, TypeCheckRecursionException),
                      Location (Location), Position (Position), compile)

import           Control.Exception.Safe (catch)
import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List              as L
import qualified Data.List.NonEmpty     as N
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.IO.Utf8      as TU
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as O
import           System.Console.ANSI    (Color (Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                                         SGR (Reset, SetColor), hSetSGR)
import           System.Directory       (createDirectoryIfMissing, doesFileExist)
import           System.Exit            (exitFailure)
import           System.FilePath        ((</>))
import qualified System.FilePath        as F
import           System.IO              (Handle, IOMode (ReadMode), hPutStr, hPutStrLn, openFile, stderr)

main :: IO ()
main = withUtf8 $ O.run_ main'

main'
  :: O.Flag "o" '["output"] "DIRECTORY" "output directory" (O.Def "." FilePath)
  -> O.Flag "l" '["library"] "PATH" "library path to find" [FilePath]
  -> O.Flag "n" '["dry-run"] "" "dry run" Bool
  -> O.Arg "SOURCE" String
  -> O.Cmd "Kmkm compiler" ()
main' output libraries dryRun src =
  catch
    do
      let
        findFile path =
          liftIO $ do
            go $ "." : O.get libraries
          where
            go (dir : dirs) = do
              let p = dir </> path
              found <- doesFileExist p
              if found then pure p else go dirs
            go [] = fail $ "not found: " ++ path
        readFile :: MonadIO m => FilePath -> m Text
        readFile = liftIO . T.readFile
        writeFile path text =
          if O.get dryRun
            then pure ()
            else do
              let path' = O.get output </> path
              liftIO $ createDirectoryIfMissing True $  F.takeDirectory path'
              liftIO $ TU.writeFile path' text
        writeLog = O.logStr 1 . T.unpack
      compile findFile readFile writeFile writeLog =<< removeFileExtension "s.km" (O.get src)
    $ \e ->
        liftIO $ do
          case e of
            ParseException m -> do
              hPutStrLn stderr "parsing error:"
              hPutStr stderr m
            NameResolveUnknownIdentifierException i l -> do
              T.hPutStrLn stderr $ "name-resolving error: unknown identifier error: " <> i
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "The identifier may not imported or may have wrong letters."
            TypeCheckNotFoundException i l -> do
              T.hPutStrLn stderr $ "type-checking error: not found error: " <> i
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "The identifier may not imported or may have wrong letters."
            TypeCheckMismatchException e a l -> do
              T.hPutStrLn stderr $ "type-checking error: mismatch error: expected: " <> e <> ", actual: " <> a
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "The expected type and actual one are different."
            TypeCheckPrimitiveTypeException i l -> do
              T.hPutStrLn stderr $ "type-checking error: primitive type not imported error: " <> i
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "A primitive is used but its type is not imported."
            TypeCheckBindProcedureEndException l -> do
              T.hPutStrLn stderr "type-checking error: bind procedure end error"
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "An end of a procedure must be a binding step."
            TypeCheckRecursionException is -> do
              T.hPutStrLn stderr $ "type-checking error: recursion error: " <> T.intercalate ", " (S.toList is)
              T.hPutStrLn stderr "Some recursion definitions are found but they have no type annotations."
            IntermediateCEmbeddedParameterMismatchException e a l -> do
              T.hPutStrLn stderr $ "intermediate C error: C embedded parameter mismatch error: expected: " <> T.pack (show e) <> ", actual: " <> T.pack (show a)
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "A number of parameters of embedded C is different from one of its type."
            CompileRecursionException ms -> do
              T.hPutStrLn stderr $ "compile error: recursion error: " <> T.intercalate ", " (N.toList ms)
              T.hPutStrLn stderr "Modules' dependency have a recursion while compiling."
            CompileModuleNameMismatchException f m l -> do
              T.hPutStrLn stderr $ "compile error: module name mismatch error: file name: " <> T.pack f <> ", module name: " <> m
              maybe (pure ()) (printLocation stderr) l
              T.hPutStrLn stderr "A file's name and its enclosed module's name is mismatched while compiling."
            CompileDotDotPathException f -> do
              T.hPutStrLn stderr $ "compile error: \"..\" path error: " <> T.pack f
              T.hPutStrLn stderr "A file path contains \"..\" while compiling."
          exitFailure

removeFileExtension :: MonadFail m => String -> FilePath -> m FilePath
removeFileExtension ext path = do
  case L.splitAt (length path - length ext - 1) path of
    (f, e) | e == '.' : ext -> pure f
    _                       -> fail $ "extension is not \"" ++ ext ++ "\""

printLocation :: MonadIO m => Handle -> Location -> m ()
printLocation outHandle (Location filePath (Position beginLine beginColumn) (Position endLine endColumn)) =
  liftIO $ do
    handle <- openFile filePath ReadMode
    hSkipLines handle $ beginLine - 1
    ls <- replicateM (endLine' - beginLine' + 1) $ T.hGetLine handle
    let
      us = underlines ls
      nd = maximum $ length . show <$> [beginLine, endLine]
      gutter = 1 + nd + 1
    hPutStrLn outHandle filePath
    T.hPutStrLn outHandle $ T.replicate gutter " " <> "| "
    alternate
      do zipWith (\l n -> T.hPutStrLn outHandle $ marginLeft gutter (T.pack (show n) <> " ") <> "| " <> l) ls [beginLine .. endLine]
      do
        flip fmap us $ \u -> do
          T.hPutStr outHandle $ T.replicate gutter " " <> "| "
          hPutStrLnRed outHandle $ "" <> u
  where
    hSkipLines _ 0      = pure ()
    hSkipLines handle n = T.hGetLine handle >> hSkipLines handle (n - 1)
    beginLine', beginColumn', endLine', endColumn' :: Int
    beginLine' = fromIntegral beginLine
    beginColumn' = fromIntegral beginColumn
    endLine' = fromIntegral endLine
    endColumn' = fromIntegral endColumn
    underlines ls =
      zipWith underline [beginLine .. endLine] $ T.length <$> ls
      where
        underline n l
          | n == beginLine && n == endLine = T.replicate (beginColumn' - 1) " " <> T.replicate (endColumn' - beginColumn') "^"
          | n == beginLine                 = T.replicate (beginColumn' - 1) " " <> T.replicate (l - beginColumn') "^"
          | n == endLine                   = T.replicate (endColumn' - 1) "^"
          | otherwise                      = T.replicate l "^"
    alternate [] bs         = sequence_ bs
    alternate as []         = sequence_ as
    alternate (a:as) (b:bs) = a >> b >> alternate as bs
    marginLeft n s = T.replicate (fromIntegral n - T.length s) " " <> s

hPutStrLnRed :: Handle -> Text -> IO ()
hPutStrLnRed h s = do
  hSetSGR h [SetColor Foreground Vivid Red]
  T.hPutStrLn h s
  hSetSGR h [Reset]
