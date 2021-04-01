{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Base
  ( Identifier (..)
  ) where

import Data.Hashable (Hashable)
import Data.Text     (Text)
import GHC.Generics  (Generic)

newtype Identifier
  = Identifier Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Identifier
