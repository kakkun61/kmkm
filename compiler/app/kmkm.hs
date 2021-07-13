{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

import Language.Kmkm (Position (Position), Pretty (pretty), compile, pattern NameResolveUnknownIdentifierException,
                      pattern ParseException, pattern TypeCheckNotFoundException)

import           Control.Exception.Safe (Handler (Handler), catches)
import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List              as L
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
import           System.IO              (Handle, IOMode (ReadMode), hPutStrLn, openFile, stderr)

main :: IO ()
main = withUtf8 $ O.run_ main'

main'
  :: O.Flag "o" '["output"] "DIRECTORY" "output directory" (O.Def "." FilePath)
  -> O.Flag "l" '["library"] "PATH" "library path to find" [FilePath]
  -> O.Flag "n" '["dry-run"] "" "dry run" Bool
  -> O.Arg "SOURCE" String
  -> O.Cmd "Kmkm compiler" ()
main' output libraries dryRun src =
  catches
    do
      let
        readFile path =
          liftIO $ do
            path' <- find $ "." : O.get libraries
            T.readFile path'
          where
            find (dir : dirs) = do
              let p = dir </> path
              found <- doesFileExist p
              if found then pure p else find dirs
            find [] = fail $ "not found: " ++ path
        writeFile path text =
          if O.get dryRun
            then pure ()
            else do
              let path' = O.get output </> path
              liftIO $ createDirectoryIfMissing True $  F.takeDirectory path'
              liftIO $ TU.writeFile path' text
        writeLog = O.logStr 1 . T.unpack
      compile readFile writeFile writeLog =<< removeFileExtension "s.km" (O.get src)
    [ Handler $ \(ParseException m) ->
        liftIO $ do
          hPutStrLn stderr "Parse error:"
          hPutStrLn stderr m
          exitFailure
    , Handler $ \(NameResolveUnknownIdentifierException i r) ->
        liftIO $ do
          T.hPutStrLn stderr $ "Unknown identifier error: " <> pretty i
          maybe (pure ()) (printRange stderr (O.get src)) r
          exitFailure
    , Handler $ \case
        TypeCheckNotFoundException i r ->
          liftIO $ do
            T.hPutStrLn stderr $ "Not found error: " <> pretty i
            maybe (pure ()) (printRange stderr (O.get src)) r
            exitFailure
        _ -> undefined
    ]

removeFileExtension :: MonadFail m => String -> FilePath -> m FilePath
removeFileExtension ext path = do
  case L.splitAt (length path - length ext - 1) path of
    (f, e) | e == '.' : ext -> pure f
    _                       -> fail $ "extension is not \"" ++ ext ++ "\""

printRange :: MonadIO m => Handle -> FilePath -> (Position, Position) -> m ()
printRange outHandle filePath (Position beginLine beginColumn, Position endLine endColumn) =
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
          | n == beginLine = T.replicate (beginColumn' - 1) " " <> T.replicate (l - beginColumn') "^"
          | n == endLine   = T.replicate endColumn' "^"
          | otherwise      = T.replicate l "^"
    alternate [] bs         = sequence_ bs
    alternate as []         = sequence_ as
    alternate (a:as) (b:bs) = a >> b >> alternate as bs
    marginLeft n s =
      T.replicate (fromIntegral n - T.length s) " " <> s

hPutStrLnRed :: Handle -> Text -> IO ()
hPutStrLnRed h s = do
  hSetSGR h [SetColor Foreground Vivid Red]
  T.hPutStrLn h s
  hSetSGR h [Reset]
