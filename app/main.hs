{-# LANGUAGE DataKinds #-}

import           Control.Monad.IO.Class    (MonadIO (liftIO))
import qualified Data.List                 as L
import qualified Data.Text.IO.Utf8         as T
import           Language.Kmkm.Builder.C   (build)
import           Language.Kmkm.Parser.Sexp (parse)
import           Main.Utf8                 (withUtf8)
import qualified Options.Declarative       as O
import           System.FilePath           ((</>))
import qualified Text.PrettyPrint          as P

main :: IO ()
main = withUtf8 $ O.run_ main'

main'
  :: O.Flag "d" '["destination"] "DIRECTORY" "destination directory" (O.Def "." String)
  -> O.Arg "SOURCE" String
  -> O.Cmd "Kmkm compiler" ()
main' dest src =
  liftIO $ do
    srcText <- T.readFile $ O.get src
    srcTree <- parse (O.get src) srcText
    doc <- build srcTree
    let destText = P.render doc
    case L.splitAt (length (O.get src) - 5) (O.get src) of
      (path, ".s.km") -> writeFile (O.get dest </> path ++ ".c") destText
      _               -> fail "extension is not \"s.km\""
