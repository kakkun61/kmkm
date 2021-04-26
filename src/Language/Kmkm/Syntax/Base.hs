{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Base
  ( Identifier (..)
  , Curriness (..)
  , Typing (..)
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

data Typing
 = Typed
 | Untyped
  deriving (Show, Read, Eq, Ord, Generic)
