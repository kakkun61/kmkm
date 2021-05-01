{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Kmkm.Syntax.Base
  ( Identifier (..)
  , ModuleName (..)
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  ) where

import Data.Hashable (Hashable)
import Data.String   (IsString (fromString))
import Data.Text     (Text)
import GHC.Generics  (Generic)

data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Identifier

instance IsString Identifier where
  fromString = UserIdentifier . fromString

newtype ModuleName = ModuleName Text deriving (Show, Read, Eq, Ord, Generic, IsString)

data Currying
  = Curried
  | Uncurried
  deriving (Show, Read, Eq, Ord, Generic)

data Typing
 = Typed
 | Untyped
  deriving (Show, Read, Eq, Ord, Generic)

data LambdaLifting
  = LambdaLifted
  | LambdaUnlifted
  deriving (Show, Read, Eq, Ord, Generic)
