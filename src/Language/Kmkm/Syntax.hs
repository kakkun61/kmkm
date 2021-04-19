{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax
  ( Module (..)
  , Member (..)
  , Bind (..)
  ) where

import Language.Kmkm.Syntax.Base  (Identifier)
import Language.Kmkm.Syntax.Type  (Type)
import Language.Kmkm.Syntax.Value (Term)

import GHC.Generics (Generic)

data Module function application arrow callee =
  Module Identifier [Member function application arrow callee]
  deriving (Show, Read, Eq, Ord, Generic)

data Member function application arrow callee
  = Definition Identifier [(Identifier, [(Identifier, Type arrow)])]
  | Bind (Bind function application arrow callee)
  deriving (Show, Read, Eq, Ord, Generic)

data Bind function application arrow callee
  = Type Identifier (Type arrow)
  | Term Identifier (Term function application callee) (Type arrow)
  deriving (Show, Read, Eq, Ord, Generic)
