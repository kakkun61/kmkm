{-# LANGUAGE DataKinds #-}

import Language.Kmkm (compile)

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Default.Class     (def)
import qualified Data.List              as L
import qualified Data.Text.IO.Utf8      as T
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as O
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory, (</>))

main :: IO ()
main = withUtf8 $ O.run_ main'

main'
  :: O.Flag "o" '["output"] "DIRECTORY" "output directory" (O.Def "." String)
  -> O.Arg "SOURCE" String
  -> O.Cmd "Kmkm compiler" ()
main' dest src =
  liftIO $ do
    srcText <- T.readFile $ O.get src
    destText <- compile def (O.get src) srcText
    case L.splitAt (length (O.get src) - 5) (O.get src) of
      (path, ".s.km") -> do
        let outputFile = O.get dest </> path ++ ".c"
        createDirectoryIfMissing True $ takeDirectory outputFile
        T.writeFile outputFile destText
      _  -> fail "extension is not \"s.km\""
