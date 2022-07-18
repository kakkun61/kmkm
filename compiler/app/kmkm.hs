{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import Language.Kmkm (Exception (CompileDotDotPathException, CompileModuleNameMismatchException, CompileRecursionException, IntermediateCEmbeddedParameterMismatchException, NameResolveUnknownIdentifierException, ParseException, TypeCheckBindProcedureEndException, TypeCheckMismatchException, TypeCheckNotFoundException, TypeCheckPrimitiveTypeException, TypeCheckRecursionException),
                      Location (Location), ParseExceptionMessage (ParseSexpMessage, ParseTextMessage),
                      Position (Position), compile)

import           Control.Exception.Safe (catch)
import           Control.Monad          (replicateM, when)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Char              (toLower)
import           Data.Foldable          (for_)
import qualified Data.List              as L
import qualified Data.List.NonEmpty     as N
import           Data.Monoid            (Last (Last, getLast))
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.IO.Utf8      as TU
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as O
import           System.Console.ANSI    (Color (Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                                         SGR (Reset, SetColor), hSetSGR)
import           System.Directory       (createDirectoryIfMissing, doesFileExist, setCurrentDirectory)
import           System.Exit            (exitFailure)
import           System.FilePath        ((</>))
import qualified System.FilePath        as F
import qualified System.FilePath.Glob   as G
import           System.IO              (Handle, IOMode (ReadMode), hPutStrLn, openFile, stderr)

#ifdef CYGPATH
import System.Process (callProcess, readProcess)
#else
import System.Process (callProcess)
#endif

type Verbosity = Int

data Step
  = Compile
  | C
  deriving (Show, Read, Eq, Ord, Enum)

instance O.ArgRead Step where
  argRead ss =
    getLast $ mconcat $ Last . read' <$> ss
    where
      read' s =
        case toLower <$> s of
          "compile" -> Just Compile
          "c"       -> Just C
          _         -> Nothing

main :: IO ()
main = withUtf8 $ O.run_ main'

main'
  :: O.Flag "o" '["output"] "PATH" "output executable file path that is relative to --directory" (O.Def "prog" FilePath)
  -> O.Flag "d" '["directory"] "PATH" "output directory path" (O.Def "out" FilePath)
  -> O.Flag "l" '["library"] "PATH" "library path to find" [FilePath]
  -> O.Flag "n" '["dry-run"] "" "dry run" Bool
  -> O.Flag "s" '["step"] "STEP" "to which step to run (compile, c)" (O.Def "c" Step)
  -> O.Flag "c" '["compiler"] "COMMAND" "compiler command" (O.Def "gcc" FilePath)
  -> O.Arg "SOURCE" String
  -> O.Cmd "Kmkm compiler" ()
main' output directory libraries dryRun step compiler src = do
  verbosity <- O.getVerbosity
  liftIO $ do
    compile' (O.get directory) (O.get libraries) (O.get dryRun) verbosity (O.get src)
    when (not (O.get dryRun) && C <= O.get step) $
      gcc (O.get compiler) (O.get output) (O.get directory) verbosity

compile' :: FilePath -> [FilePath] -> Bool -> Verbosity -> String -> IO ()
compile' output libraries dryRun verbosity src =
  catch
    do
      let
        findFile path =
          go $ "." : libraries
          where
            go (dir : dirs) = do
              let p = dir </> path
              found <- doesFileExist p
              if found then pure p else go dirs
            go [] = fail $ "not found: " ++ path
        writeFile path text =
          if dryRun
            then pure ()
            else do
              let path' = output </> path
              createDirectoryIfMissing True $ F.takeDirectory path'
              TU.writeFile path' text
        writeLog = when (1 <= verbosity) . T.putStrLn
      compile findFile T.readFile writeFile writeLog =<< removeFileExtension "s.km" src
    $ \e -> do
          case e of
            ParseException ms -> do
              hPutStrLn stderr "parsing text error:"
              let count = length ms
              for_ (zip ms [1 ..]) $ \(m, i) -> do
                hPutStrLn stderr $ "possible fix #" ++ show (i :: Word) ++ "/" ++ show count ++ ":"
                case m of
                  ParseTextMessage m ->
                    hPutStrLn stderr m
                  ParseSexpMessage m l -> do
                    hPutStrLn stderr m
                    maybe (pure ()) (printLocation stderr) l
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
removeFileExtension ext path =
  case L.splitAt (length path - length ext - 1) path of
    (f, e) | e == '.' : ext -> pure f
    _                       -> fail $ "extension is not \"" ++ ext ++ "\""

printLocation :: Handle -> Location -> IO ()
printLocation outHandle (Location filePath (Position beginLine beginColumn) (Position endLine endColumn)) = do
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

gcc :: FilePath -> FilePath -> FilePath -> Verbosity -> IO ()
gcc cc output directory verbosity = do
  setCurrentDirectory directory
  sourceFiles <- traverse convertPath =<< G.glob "**/*.c"
  let verbosity' = ["-" ++ replicate verbosity 'v' | verbosity /= 0]
  when (0 < verbosity) $ putStrLn $ unwords $ cc : (sourceFiles ++ verbosity' ++ ["-o", output])
  callProcess cc (sourceFiles ++ verbosity' ++ ["-o", output])

convertPath :: FilePath -> IO FilePath
#ifdef CYGPATH
convertPath path = init <$> readProcess "cygpath" [path] "" -- empty string is something wrong.
#else
convertPath = pure
#endif
