{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Parse.Sexp.C
  ( embeddedParser
  ) where

import qualified Language.Kmkm.Internal.Parse.Sexp as R
import qualified Language.Kmkm.Internal.Syntax     as S

import           Data.Functor.With                  (MayHave)
import qualified Data.Functor.With                  as W
import           Data.Traversable                   (for)
import qualified Language.Kmkm.Internal.Syntax.Sexp as S

embeddedParser :: (Traversable f, MayHave S.Location f) => R.EmbeddedParser f
embeddedParser = R.EmbeddedParser value type'

value :: (Traversable f, MayHave S.Location f) => f (S.Sexp f) -> Either [R.Exception] (f (S.EmbeddedValue f))
value s =
  for s $ \case
    S.List [sv, si, sps, sb] -> do
      R.symbol "c-value" "value" sv
      i <- R.string si
      ps <- R.list R.string sps
      b <- R.string sb
      pure $ S.EmbeddedValueC $ S.EmbeddedCValue i ps b
    _ -> Left [R.SexpException "unexpected format" "value" $ W.mayGet s]

type' :: (Traversable f, MayHave S.Location f) => f (S.Sexp f) -> Either [R.Exception] (f (S.EmbeddedType f))
type' s =
  for s $ \case
    S.List [st, si, sb] -> do
      R.symbol "c-type" "type'" st
      i <- R.string si
      b <- R.string sb
      pure $ S.EmbeddedTypeC $ S.EmbeddedCType i b
    _ -> Left [R.SexpException "unexpected format" "type'" $ W.mayGet s]
