{-# LANGUAGE CPP              #-}
{-# LANGUAGE DeriveGeneric    #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
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
  = Fraction { significand :: Integer, exponent :: Integer, base :: Integer }
  | String Text
  | Bool Bool
  deriving (Show, Read, Eq, Ord, Generic)
