{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.Kmkm            (compile)
import qualified Language.Kmkm.Parse.Sexp as KS

import           Control.Exception.Safe (Handler (Handler), catches)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.IO.Utf8      as TU
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as O
import           System.Directory       (createDirectoryIfMissing, doesFileExist)
import           System.FilePath        ((</>))
import qualified System.FilePath        as F
import           System.IO              (hPutStrLn, stderr)

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
    [ Handler $ \(KS.Exception m) -> liftIO $ hPutStrLn stderr $ "Parse error:\n" ++ m ]

removeFileExtension :: MonadFail m => String -> FilePath -> m FilePath
removeFileExtension ext path = do
  case L.splitAt (length path - length ext - 1) path of
    (f, e) | e == '.' : ext -> pure f
    _                       -> fail $ "extension is not \"" ++ ext ++ "\""
