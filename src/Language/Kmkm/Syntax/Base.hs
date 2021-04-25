{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Base
  ( Identifier (..)
  , Curriness (..)
  ) where

import Data.Hashable (Hashable)
import Data.Text     (Text)
import GHC.Generics  (Generic)

newtype Identifier
  = Identifier Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Identifier

data Curriness
  = Curried
  | Uncurried
  deriving (Show, Read, Eq, Ord, Generic)
