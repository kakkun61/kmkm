module Language.Kmkm
  ( compile
  , Config (..)
  , TypeMap (..)
  ) where

import Language.Kmkm.Builder.C   (build)
import Language.Kmkm.Config      (Config (Config), TypeMap (TypeMap, byte, frac, frac2, int))
import Language.Kmkm.Parser.Sexp (parse)

import           Control.Monad.Catch (MonadThrow)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Text.PrettyPrint    as P

compile :: MonadThrow m => Config -> String -> Text -> m Text
compile config tag src = do
  tree <- parse tag src
  doc <- build config tree
  pure $ T.pack $ P.render doc
