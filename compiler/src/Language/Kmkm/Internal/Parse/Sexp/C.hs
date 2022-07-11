{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Parse.Sexp.C
  ( embeddedParser
  ) where

import qualified Language.Kmkm.Internal.Parse.Sexp as R
import qualified Language.Kmkm.Internal.Syntax     as S

import qualified Barbies.Bare                       as B
import           Data.Traversable                   (for)
import qualified Language.Kmkm.Internal.Syntax.Sexp as S

embeddedParser :: (Traversable f, S.HasLocation f) => R.EmbeddedParser f
embeddedParser = R.EmbeddedParser value type'

value :: (Traversable f, S.HasLocation f) => f (S.Sexp B.Covered f) -> Either [R.Exception] (f (S.EmbeddedValue B.Covered f))
value s =
  for s $ \case
    S.List [sv, si, sps, sb] -> do
      R.symbol "c-value" "value" sv
      i <- R.string si
      ps <- R.list R.string sps
      b <- R.string sb
      pure $ S.EmbeddedValueC $ S.EmbeddedCValue i ps b
    _ -> Left [R.SexpException "unexpected format" "value" $ S.location s]

type' :: (Traversable f, S.HasLocation f) => f (S.Sexp B.Covered f) -> Either [R.Exception] (f (S.EmbeddedType B.Covered f))
type' s =
  for s $ \case
    S.List [st, si, sb] -> do
      R.symbol "c-type" "type'" st
      i <- R.string si
      b <- R.string sb
      pure $ S.EmbeddedTypeC $ S.EmbeddedCType i b
    _ -> Left [R.SexpException "unexpected format" "type'" $ S.location s]
