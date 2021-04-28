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
  , Term' (..)
  , Literal (..)
  , Application (..)
  , Function (..)
  ) where

import Language.Kmkm.Syntax.Base (Curriness (Curried, Uncurried), Identifier, Typing (Typed, Untyped))
import Language.Kmkm.Syntax.Type (Type)

import qualified Data.Kind    as K
import           Data.Text    (Text)
import           GHC.Generics (Generic)

type Term :: Curriness -> Typing -> K.Type
data family Term c t

data instance Term c 'Typed =
  TypedTerm (Term' c 'Typed) (Type c)
  deriving Generic

newtype instance Term c 'Untyped =
  UntypedTerm (Term' c 'Untyped)
  deriving Generic

deriving instance Show (Term 'Curried 'Typed)
deriving instance Read (Term 'Curried 'Typed)
deriving instance Eq (Term 'Curried 'Typed)
deriving instance Ord (Term 'Curried 'Typed)

deriving instance Show (Term 'Curried 'Untyped)
deriving instance Read (Term 'Curried 'Untyped)
deriving instance Eq (Term 'Curried 'Untyped)
deriving instance Ord (Term 'Curried 'Untyped)

deriving instance Show (Term 'Uncurried 'Typed)
deriving instance Read (Term 'Uncurried 'Typed)
deriving instance Eq (Term 'Uncurried 'Typed)
deriving instance Ord (Term 'Uncurried 'Typed)

deriving instance Show (Term 'Uncurried 'Untyped)
deriving instance Read (Term 'Uncurried 'Untyped)
deriving instance Eq (Term 'Uncurried 'Untyped)
deriving instance Ord (Term 'Uncurried 'Untyped)

data Term' c t
  = Variable Identifier
  | Literal (Literal c t)
  | Application' (Application c t)
  deriving Generic

deriving instance (Show (Application c t), Show (Literal c t)) => Show (Term' c t)
deriving instance (Read (Application c t), Read (Literal c t)) => Read (Term' c t)
deriving instance (Eq (Application c t), Eq (Literal c t)) => Eq (Term' c t)
deriving instance (Ord (Application c t), Ord (Literal c t)) => Ord (Term' c t)

type Literal :: Curriness -> Typing -> K.Type
data Literal c t
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function' (Function c t)
  deriving Generic

deriving instance Show (Function c t) => Show (Literal c t)
deriving instance Read (Function c t) => Read (Literal c t)
deriving instance Eq (Function c t) => Eq (Literal c t)
deriving instance Ord (Function c t) => Ord (Literal c t)

type Application :: Curriness -> Typing -> K.Type
data family Application c t

data instance Application 'Curried t =
  ApplicationC (Term 'Curried t) (Term 'Curried t)
  deriving Generic

data instance Application 'Uncurried t
  = Application1 (Term 'Uncurried t) (Term 'Uncurried t)
  | Application2 (Term 'Uncurried t) (Term 'Uncurried t) (Term 'Uncurried t)
  | Application3 (Term 'Uncurried t) (Term 'Uncurried t) (Term 'Uncurried t) (Term 'Uncurried t)
  deriving Generic

deriving instance Show (Term 'Curried t) => Show (Application 'Curried t)
deriving instance Read (Term 'Curried t) => Read (Application 'Curried t)
deriving instance Eq (Term 'Curried t) => Eq (Application 'Curried t)
deriving instance (Eq (Term 'Curried t), Ord (Term 'Curried t)) => Ord (Application 'Curried t)

deriving instance Show (Term 'Uncurried t) => Show (Application 'Uncurried t)
deriving instance Read (Term 'Uncurried t) => Read (Application 'Uncurried t)
deriving instance Eq (Term 'Uncurried t) => Eq (Application 'Uncurried t)
deriving instance (Eq (Term 'Uncurried t), Ord (Term 'Uncurried t)) => Ord (Application 'Uncurried t)

type Function :: Curriness -> Typing -> K.Type
data family Function c

data instance Function 'Curried t =
  FunctionC Identifier (Type 'Curried) (Term 'Curried t)
  deriving Generic

data instance Function 'Uncurried t
  = Function1 Identifier (Type 'Uncurried) (Term 'Uncurried t)
  | Function2 Identifier (Type 'Uncurried) Identifier (Type 'Uncurried) (Term 'Uncurried t)
  | Function3 Identifier (Type 'Uncurried) Identifier (Type 'Uncurried) Identifier (Type 'Uncurried) (Term 'Uncurried t)
  deriving Generic

deriving instance Show (Term 'Curried t) => Show (Function 'Curried t)
deriving instance Read (Term 'Curried t) => Read (Function 'Curried t)
deriving instance Eq (Term 'Curried t) => Eq (Function 'Curried t)
deriving instance (Eq (Term 'Curried t), Ord (Term 'Curried t)) => Ord (Function 'Curried t)

deriving instance Show (Term 'Uncurried t) => Show (Function 'Uncurried t)
deriving instance Read (Term 'Uncurried t) => Read (Function 'Uncurried t)
deriving instance Eq (Term 'Uncurried t) => Eq (Function 'Uncurried t)
deriving instance (Eq (Term 'Uncurried t), Ord (Term 'Uncurried t)) => Ord (Function 'Uncurried t)
