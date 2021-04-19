{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Type
  ( Type (..)
  , Arrow (..)
  , ArrowN (..)
  ) where

import Language.Kmkm.Syntax.Base (Identifier)

import GHC.Generics (Generic)

data Type arrow
  = Variable Identifier
  | Application (Type arrow) (Type arrow)
  | Arrow' arrow
  deriving (Show, Read, Eq, Ord, Generic)

data Arrow =
  Arrow (Type Arrow) (Type Arrow)
  deriving (Show, Read, Eq, Ord, Generic)

data ArrowN
  = Arrow1 (Type ArrowN) (Type ArrowN)
  | Arrow2 (Type ArrowN) (Type ArrowN) (Type ArrowN)
  | Arrow3 (Type ArrowN) (Type ArrowN) (Type ArrowN) (Type ArrowN)
  deriving (Show, Read, Eq, Ord, Generic)
