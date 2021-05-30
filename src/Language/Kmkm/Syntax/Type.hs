{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Language.Kmkm.Syntax.Type
  ( Type (..)
  , Arrow (..)
  ) where

import Language.Kmkm.Syntax.Base (Currying (Curried, Uncurried), Identifier)

import qualified Data.Kind    as K
import           GHC.Generics (Generic)

type Type :: Currying -> K.Type
data Type c
  = Variable Identifier
  | Application (Type c) (Type c)
  | Arrow (Arrow c)
  deriving Generic

deriving instance Show (Arrow c) => Show (Type c)
deriving instance Read (Arrow c) => Read (Type c)
deriving instance Eq (Arrow c) => Eq (Type c)
deriving instance Ord (Arrow c) => Ord (Type c)

type Arrow :: Currying -> K.Type
data family Arrow c

data instance Arrow 'Curried =
  ArrowC (Type 'Curried) (Type 'Curried)
  deriving (Show, Read, Eq, Ord, Generic)

data instance Arrow 'Uncurried
  = Arrow0 (Type 'Uncurried)
  | Arrow1 (Type 'Uncurried) (Type 'Uncurried)
  | Arrow2 (Type 'Uncurried) (Type 'Uncurried) (Type 'Uncurried)
  | Arrow3 (Type 'Uncurried) (Type 'Uncurried) (Type 'Uncurried) (Type 'Uncurried)
  deriving (Show, Read, Eq, Ord, Generic)
