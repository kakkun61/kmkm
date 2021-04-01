{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax
  ( Module (..)
  , Member (..)
  , Alias (..)
  ) where

import Language.Kmkm.Syntax.Base  (Identifier)
import Language.Kmkm.Syntax.Type  (Type)
import Language.Kmkm.Syntax.Value (Term)

import GHC.Generics (Generic)

data Module =
  Module Identifier [Member]
  deriving (Show, Read, Eq, Ord, Generic)

data Member
  = Definition Identifier [(Identifier, [(Identifier, Type)])]
  | Alias Alias
  deriving (Show, Read, Eq, Ord, Generic)

data Alias
  = Type Identifier Type
  | Term Identifier Term Type
  deriving (Show, Read, Eq, Ord, Generic)
