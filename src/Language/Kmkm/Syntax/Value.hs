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

data Term function application
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

data Application function =
  Application (Term function (Application function)) (Term function (Application function))
  deriving (Show, Read, Eq, Ord, Generic)

data ApplicationN function
  = Application1 (Term function (ApplicationN function)) (Term function (ApplicationN function))
  | Application2 (Term function (ApplicationN function)) (Term function (ApplicationN function)) (Term function (ApplicationN function))
  | Application3 (Term function (ApplicationN function)) (Term function (ApplicationN function)) (Term function (ApplicationN function)) (Term function (ApplicationN function))
  deriving (Show, Read, Eq, Ord, Generic)

data Function application =
  Function Identifier (Term (Function application) application)
  deriving (Show, Read, Eq, Ord, Generic)

data FunctionN application
  = Function1 Identifier (Term (FunctionN application) application)
  | Function2 Identifier Identifier (Term (FunctionN application) application)
  | Function3 Identifier Identifier Identifier (Term (FunctionN application) application)
  deriving (Show, Read, Eq, Ord, Generic)
