{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Exception
  ( Exception (..)
  ) where

import qualified Control.Exception as E
import           GHC.Generics      (Generic)

newtype Exception = Exception String deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception
