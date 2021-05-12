module Language.Kmkm
  ( compile
  ) where

import Language.Kmkm.Builder.C   (build)
import Language.Kmkm.Parser.Sexp (parse)

import           Control.Monad.Catch (MonadThrow)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Text.PrettyPrint    as P

compile :: MonadThrow m => String -> Text -> m Text
compile tag src = do
  tree <- parse tag src
  doc <- build tree
  pure $ T.pack $ P.render doc
