{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors         #-}
#else
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm.Syntax.Value
  ( Term (..)
  , Literal (..)
  , Application (..)
  , Function (..)
  ) where

import Language.Kmkm.Syntax.Base (Curriness (Curried, Uncurried), Identifier)

import Data.Kind    (Type)
import Data.Text    (Text)
import GHC.Generics (Generic)

type Term :: Curriness -> Type
data Term c
  = Variable Identifier
  | Literal (Literal c)
  | Application' (Application c)
  deriving Generic

deriving instance (Show (Application c), Show (Literal c)) => Show (Term c)
deriving instance (Read (Application c), Read (Literal c)) => Read (Term c)
deriving instance (Eq (Application c), Eq (Literal c)) => Eq (Term c)
deriving instance (Ord (Application c), Ord (Literal c)) => Ord (Term c)

type Literal :: Curriness -> Type
data Literal c
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function' (Function c)
  deriving Generic

deriving instance Show (Function c) => Show (Literal c)
deriving instance Read (Function c) => Read (Literal c)
deriving instance Eq (Function c) => Eq (Literal c)
deriving instance Ord (Function c) => Ord (Literal c)

type Application :: Curriness -> Type
data family Application c

data instance Application 'Curried =
  ApplicationC (Term 'Curried) (Term 'Curried)
  deriving (Show, Read, Eq, Ord, Generic)

data instance Application 'Uncurried
  = Application1 (Term 'Uncurried) (Term 'Uncurried)
  | Application2 (Term 'Uncurried) (Term 'Uncurried) (Term 'Uncurried)
  | Application3 (Term 'Uncurried) (Term 'Uncurried) (Term 'Uncurried) (Term 'Uncurried)
  deriving (Show, Read, Eq, Ord, Generic)

type Function :: Curriness -> Type
data family Function c

data instance Function 'Curried =
  FunctionC Identifier (Term 'Curried)
  deriving (Show, Read, Eq, Ord, Generic)

data instance Function 'Uncurried
  = Function1 Identifier (Term 'Uncurried)
  | Function2 Identifier Identifier (Term 'Uncurried)
  | Function3 Identifier Identifier Identifier (Term 'Uncurried)
  deriving (Show, Read, Eq, Ord, Generic)
