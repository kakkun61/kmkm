{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Language.Kmkm.Syntax.Type
  ( Type (..)
  , Arrow
  , ArrowC (..)
  , ArrowN (..)
  ) where

import Language.Kmkm.Syntax.Base (Curriness (Curried, Uncurried), Identifier)

import qualified Data.Kind    as K
import           GHC.Generics (Generic)

type Type :: Curriness -> K.Type
data Type c
  = Variable Identifier
  | Application (Type c) (Type c)
  | Arrow' (Arrow c)
  deriving Generic

deriving instance Show (Arrow c) => Show (Type c)
deriving instance Read (Arrow c) => Read (Type c)
deriving instance Eq (Arrow c) => Eq (Type c)
deriving instance Ord (Arrow c) => Ord (Type c)

type Arrow :: Curriness -> K.Type
type family Arrow c where
  Arrow 'Curried = ArrowC
  Arrow 'Uncurried = ArrowN

data ArrowC =
  Arrow (Type 'Curried) (Type 'Curried)
  deriving (Show, Read, Eq, Ord, Generic)

data ArrowN
  = Arrow1 (Type 'Uncurried) (Type 'Uncurried)
  | Arrow2 (Type 'Uncurried) (Type 'Uncurried) (Type 'Uncurried)
  | Arrow3 (Type 'Uncurried) (Type 'Uncurried) (Type 'Uncurried) (Type 'Uncurried)
  deriving (Show, Read, Eq, Ord, Generic)
