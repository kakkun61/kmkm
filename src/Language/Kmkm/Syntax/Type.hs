{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Language.Kmkm.Syntax.Type
  ( Type (..)
  , Function (..)
  ) where

import Language.Kmkm.Syntax.Base (Currying (Curried, Uncurried), Identifier)

import qualified Data.Kind    as K
import           GHC.Generics (Generic)

type Type :: Currying -> K.Type
data Type c
  = Variable Identifier
  | Application (Type c) (Type c)
  | Function (Function c)
  | Procedure (Type c)
  deriving Generic

deriving instance Show (Function c) => Show (Type c)
deriving instance Read (Function c) => Read (Type c)
deriving instance Eq (Function c) => Eq (Type c)
deriving instance Ord (Function c) => Ord (Type c)

type Function :: Currying -> K.Type
data family Function c

data instance Function 'Curried =
  FunctionC (Type 'Curried) (Type 'Curried)
  deriving (Show, Read, Eq, Ord, Generic)

data instance Function 'Uncurried =
  FunctionN [Type 'Uncurried] (Type 'Uncurried)
  deriving (Show, Read, Eq, Ord, Generic)
