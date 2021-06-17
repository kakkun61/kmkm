{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Base
  ( Identifier (..)
  , QualifiedIdentifier (..)
  , ModuleName (..)
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  ) where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Data.String        (IsString (fromString))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = UserIdentifier . fromString

data QualifiedIdentifier =
  QualifiedIdentifier (Maybe ModuleName) Identifier
  deriving (Show, Read, Eq, Ord, Generic)

newtype ModuleName = ModuleName (N.NonEmpty Text) deriving (Show, Read, Eq, Ord, Generic)

instance IsString ModuleName where
  fromString = ModuleName . (:| []) . fromString

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
