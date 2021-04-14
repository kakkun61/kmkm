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
  ) where

import Language.Kmkm.Syntax.Base (Identifier)

import Data.Text    (Text)
import GHC.Generics (Generic)

data Term
  = Variable Identifier
  | Literal Literal
  | Application Term Term
  deriving (Show, Read, Eq, Ord, Generic)

data Literal
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)
