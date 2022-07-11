{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Kmkm.Internal.Syntax.Sexp.Token
  ( Token (..)
  , Literal (..)
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)

data Token
  = Module
  | Define
  | BindValue
  | BindValueForeign
  | BindTypeForeign
  | Type
  | Function
  | Procedure
  | Apply
  | Call
  | Let
  | ForAll
  | CValue
  | CType
  | Identifier Text
  | Literal Literal
  deriving (Show, Read, Eq, Ord, Generic)

data Literal
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)
