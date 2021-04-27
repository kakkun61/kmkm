{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Base
  ( Identifier (..)
  , Curriness (..)
  , Typing (..)
  ) where

import Data.Hashable (Hashable)
import Data.String   (IsString (fromString))
import Data.Text     (Text)
import GHC.Generics  (Generic)

data Identifier
  = UserIdentifier Text
  | SystemIdentifier Word
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Identifier

instance IsString Identifier where
  fromString = UserIdentifier . fromString

data Curriness
  = Curried
  | Uncurried
  deriving (Show, Read, Eq, Ord, Generic)

data Typing
 = Typed
 | Untyped
  deriving (Show, Read, Eq, Ord, Generic)
