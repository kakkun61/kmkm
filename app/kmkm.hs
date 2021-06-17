{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.Kmkm             (Config (Config), TypeMap (TypeMap), compile)
import qualified Language.Kmkm.Parser.Sexp as KS

import           Control.Exception.Safe (Handler (Handler), catches)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Default.Class     (def)
import qualified Data.List              as L
import           Data.Text              (Text)
import qualified Data.Text.IO.Utf8      as T
import qualified Dhall                  as D
import           Language.Kmkm.Syntax   (CHeader (LocalHeader, SystemHeader))
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as O
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))
import qualified System.FilePath        as F
import           System.IO              (hPutStrLn, stderr)

main :: IO ()
main = withUtf8 $ O.run_ main'

main'
  :: O.Flag "o" '["output"] "DIRECTORY" "output directory" (O.Def "." String)
  -> O.Flag "c" '["config"] "FILE" "configuration file" (Maybe String)
  -> O.Arg "SOURCE" String
  -> O.Cmd "Kmkm compiler" ()
main' dest config src =
  liftIO $
    catches
      do
        config' <-
          case O.get config of
            Nothing -> pure def
            Just c -> do
              t <- T.readFile c
              decodeConfig t
        let
          writeFile path text = do
            let path' = O.get dest </> path
            createDirectoryIfMissing True $ F.takeDirectory path'
            T.writeFile path' text
        compile config' T.readFile writeFile =<< removeFileExtension "s.km" (O.get src)
      [ Handler $ \(KS.Exception m) -> hPutStrLn stderr $ "Parse error:\n" ++ m ]

removeFileExtension :: MonadFail m => String -> FilePath -> m FilePath
removeFileExtension ext path = do
  case L.splitAt (length path - length ext - 1) path of
    (f, e) | e == '.' : ext -> pure f
    _                       -> fail $ "extension is not \"" ++ ext ++ "\""

decodeConfig :: Text -> IO Config
decodeConfig =
  D.input config
  where
    config =
      D.record $
        Config
          <$> D.field "headers" (D.list header)
          <*> D.field "typeMap" typeMap
    header =
      D.union $
        (SystemHeader <$> D.constructor "SystemHeader" D.strictText)
        <> (LocalHeader <$> D.constructor "LocalHeader" D.strictText)
    typeMap =
      D.record $
        TypeMap
          <$> D.field "int" D.strictText
          <*> D.field "uint" D.strictText
          <*> D.field "byte" D.strictText
          <*> D.field "frac" D.strictText
          <*> D.field "frac2" D.strictText
