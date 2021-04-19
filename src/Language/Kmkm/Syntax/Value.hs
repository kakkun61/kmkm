{-# LANGUAGE CPP              #-}
{-# LANGUAGE DeriveGeneric    #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#else
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm.Syntax.Value
  ( Term (..)
  , Literal (..)
  , Application (..)
  , ApplicationN (..)
  , Function (..)
  , FunctionN (..)
  ) where

import Language.Kmkm.Syntax.Base (Identifier)

import Data.Text    (Text)
import GHC.Generics (Generic)

data Term function application callee
  = Variable Identifier
  | Literal (Literal function)
  | Application' application
  deriving (Show, Read, Eq, Ord, Generic)

data Literal function
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function' function
  deriving (Show, Read, Eq, Ord, Generic)

data Application function callee =
  Application callee (Term function (Application function callee) callee)
  deriving (Show, Read, Eq, Ord, Generic)

data ApplicationN function callee
  = Application1 callee (Term function (ApplicationN function callee) callee)
  | Application2 callee (Term function (ApplicationN function callee) callee) (Term function (ApplicationN function callee) callee)
  | Application3 callee (Term function (ApplicationN function callee) callee) (Term function (ApplicationN function callee) callee) (Term function (ApplicationN function callee) callee)
  deriving (Show, Read, Eq, Ord, Generic)

data Function application callee =
  Function Identifier (Term (Function application callee) application callee)
  deriving (Show, Read, Eq, Ord, Generic)

data FunctionN application callee
  = Function1 Identifier (Term (FunctionN application callee) application callee)
  | Function2 Identifier Identifier (Term (FunctionN application callee) application callee)
  | Function3 Identifier Identifier Identifier (Term (FunctionN application callee) application callee)
  deriving (Show, Read, Eq, Ord, Generic)
