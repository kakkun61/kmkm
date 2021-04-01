{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Type
  ( Type (..)
  ) where

import Language.Kmkm.Syntax.Base (Identifier)

import GHC.Generics (Generic)

data Type
  = Variable Identifier
  | Application Type Type
  deriving (Show, Read, Eq, Ord, Generic)
