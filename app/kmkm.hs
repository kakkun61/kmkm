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
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as O
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory, (</>))
import           System.IO

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
          srcText <- T.readFile $ O.get src
          destText <- compile config' (O.get src) srcText
          case L.splitAt (length (O.get src) - 5) (O.get src) of
            (path, ".s.km") -> do
              let outputFile = O.get dest </> path ++ ".c"
              createDirectoryIfMissing True $ takeDirectory outputFile
              T.writeFile outputFile destText
            _  -> fail "extension is not \"s.km\""
      [ Handler $ \(KS.Exception m) -> hPutStrLn stderr $ "Parse error:\n" ++ m ]

decodeConfig :: Text -> IO Config
decodeConfig =
  D.input decoder
  where
    decoder =
      D.record $
        Config
          <$> D.field "headers" (D.list D.string)
          <*> D.field "typeMap" typeMap
    typeMap =
      D.record $
        TypeMap
          <$> D.field "int" D.strictText
          <*> D.field "uint" D.strictText
          <*> D.field "byte" D.strictText
          <*> D.field "frac" D.strictText
          <*> D.field "frac2" D.strictText
