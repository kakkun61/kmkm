{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Parse.Sexp.C
  ( embeddedParser
  ) where

import           Language.Kmkm.Internal.Parse.Sexp (EmbeddedType, EmbeddedValue, Parser, list, string, withPosition)
import qualified Language.Kmkm.Internal.Parse.Sexp as R
import qualified Language.Kmkm.Internal.Syntax     as S

import           Control.Monad     (void)
import qualified Text.Parser.Token as P

embeddedParser :: R.EmbeddedParser S.EmbeddedCType S.EmbeddedCValue
embeddedParser = R.EmbeddedParser embeddedValue embeddedType

embeddedValue :: Parser et ev (S.WithLocation EmbeddedValue)
embeddedValue =
  withPosition $ do
    void $ P.textSymbol "c-value"
    i <- withPosition string
    ps <- list $ withPosition string
    b <- withPosition string
    pure $ S.EmbeddedValueC $ S.EmbeddedCValue i ps b

embeddedType :: Parser et ev (S.WithLocation EmbeddedType)
embeddedType =
  withPosition $ do
    void $ P.textSymbol "c-type"
    i <- withPosition string
    b <- withPosition string
    pure $ S.EmbeddedTypeC $ S.EmbeddedCType i b
