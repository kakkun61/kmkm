{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE EmptyDataDeriving        #-}
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
  , ProcedureStep (..)
  , Literal (..)
  , Application (..)
  , Function (..)
  , TypeAnnotation (..)
  ) where

import           Language.Kmkm.Syntax.Base (Currying (Curried, Uncurried), Identifier,
                                            LambdaLifting (LambdaLifted, LambdaUnlifted), QualifiedIdentifier,
                                            Typing (Typed, Untyped))
import           Language.Kmkm.Syntax.Type (Type)
import qualified Language.Kmkm.Syntax.Type as T

import qualified Data.Kind          as K
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

type Term :: Currying -> LambdaLifting -> Typing -> K.Type
data family Term

data instance Term c l 'Typed =
  TypedTerm (Term' c l 'Typed) (Type c)
  deriving Generic

newtype instance Term c l 'Untyped =
  UntypedTerm (Term' c l 'Untyped)
  deriving Generic

deriving instance (Show (Function 'Curried l 'Typed), Show (TypeAnnotation 'Curried l 'Typed)) => Show (Term 'Curried l 'Typed)
deriving instance (Read (Function 'Curried l 'Typed), Read (TypeAnnotation 'Curried l 'Typed)) => Read (Term 'Curried l 'Typed)
deriving instance (Eq (Function 'Curried l 'Typed), Eq (TypeAnnotation 'Curried l 'Typed)) => Eq (Term 'Curried l 'Typed)
deriving instance (Ord (Function 'Curried l 'Typed), Ord (TypeAnnotation 'Curried l 'Typed)) => Ord (Term 'Curried l 'Typed)

deriving instance Show (Function 'Curried l 'Untyped) => Show (Term 'Curried l 'Untyped)
deriving instance Read (Function 'Curried l 'Untyped) => Read (Term 'Curried l 'Untyped)
deriving instance Eq (Function 'Curried l 'Untyped) => Eq (Term 'Curried l 'Untyped)
deriving instance Ord (Function 'Curried l 'Untyped) => Ord (Term 'Curried l 'Untyped)

deriving instance (Show (Function 'Uncurried l 'Typed), Show (TypeAnnotation 'Uncurried l 'Typed)) => Show (Term 'Uncurried l 'Typed)
deriving instance (Read (Function 'Uncurried l 'Typed), Read (TypeAnnotation 'Uncurried l 'Typed)) => Read (Term 'Uncurried l 'Typed)
deriving instance (Eq (Function 'Uncurried l 'Typed), Eq (TypeAnnotation 'Uncurried l 'Typed)) => Eq (Term 'Uncurried l 'Typed)
deriving instance (Ord (Function 'Uncurried l 'Typed), Ord (TypeAnnotation 'Uncurried l 'Typed)) => Ord (Term 'Uncurried l 'Typed)

deriving instance Show (Function 'Uncurried l 'Untyped) => Show (Term 'Uncurried l 'Untyped)
deriving instance Read (Function 'Uncurried l 'Untyped) => Read (Term 'Uncurried l 'Untyped)
deriving instance Eq (Function 'Uncurried l 'Untyped) => Eq (Term 'Uncurried l 'Untyped)
deriving instance Ord (Function 'Uncurried l 'Untyped) => Ord (Term 'Uncurried l 'Untyped)

data Term' c l t
  = Variable QualifiedIdentifier
  | Literal (Literal c l t)
  | Application (Application c l t)
  | Procedure (NonEmpty (ProcedureStep c l t))
  | TypeAnnotation (TypeAnnotation c l t)
  deriving Generic

deriving instance (Show (Application c l t), Show (Literal c l t), Show (ProcedureStep c l t), Show (TypeAnnotation c l t)) => Show (Term' c l t)
deriving instance (Read (Application c l t), Read (Literal c l t), Read (ProcedureStep c l t), Read (TypeAnnotation c l t)) => Read (Term' c l t)
deriving instance (Eq (Application c l t), Eq (Literal c l t), Eq (ProcedureStep c l t), Eq (TypeAnnotation c l t)) => Eq (Term' c l t)
deriving instance (Ord (Application c l t), Ord (Literal c l t), Ord (ProcedureStep c l t), Ord (TypeAnnotation c l t)) => Ord (Term' c l t)

type TypeAnnotation :: Currying -> LambdaLifting -> Typing -> K.Type
data family TypeAnnotation c l t

data instance TypeAnnotation c l 'Untyped =
  TypeAnnotation' (Term c l 'Untyped) (Type c)

data instance TypeAnnotation c l 'Typed

deriving instance (Show (T.Function c), Show (Term c l 'Untyped)) => Show (TypeAnnotation c l 'Untyped)
deriving instance (Read (T.Function c), Read (Term c l 'Untyped)) => Read (TypeAnnotation c l 'Untyped)
deriving instance (Eq (T.Function c), Eq (Term c l 'Untyped)) => Eq (TypeAnnotation c l 'Untyped)
deriving instance (Ord (T.Function c), Ord (Term c l 'Untyped)) => Ord (TypeAnnotation c l 'Untyped)

deriving instance (Show (T.Function c), Show (Term c l 'Untyped)) => Show (TypeAnnotation c l 'Typed)
deriving instance (Read (T.Function c), Read (Term c l 'Untyped)) => Read (TypeAnnotation c l 'Typed)
deriving instance (Eq (T.Function c), Eq (Term c l 'Untyped)) => Eq (TypeAnnotation c l 'Typed)
deriving instance (Ord (T.Function c), Ord (Term c l 'Untyped)) => Ord (TypeAnnotation c l 'Typed)

data ProcedureStep c l t
  = BindProcedure Identifier (Term c l t)
  | TermProcedure (Term c l t)

deriving instance Show (Term c l t) => Show (ProcedureStep c l t)
deriving instance Read (Term c l t) => Read (ProcedureStep c l t)
deriving instance Eq (Term c l t) => Eq (ProcedureStep c l t)
deriving instance Ord (Term c l t) => Ord (ProcedureStep c l t)

data Literal c l t
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function (Function c l t)
  deriving Generic

deriving instance Show (Function c l t) => Show (Literal c l t)
deriving instance Read (Function c l t) => Read (Literal c l t)
deriving instance Eq (Function c l t) => Eq (Literal c l t)
deriving instance Ord (Function c l t) => Ord (Literal c l t)

type Application :: Currying -> LambdaLifting -> Typing -> K.Type
data family Application

data instance Application 'Curried l t =
  ApplicationC (Term 'Curried l t) (Term 'Curried l t)
  deriving Generic

data instance Application 'Uncurried l t =
  ApplicationN (Term 'Uncurried l t) [Term 'Uncurried l t]
  deriving Generic

deriving instance Show (Term 'Curried l t) => Show (Application 'Curried l t)
deriving instance Read (Term 'Curried l t) => Read (Application 'Curried l t)
deriving instance Eq (Term 'Curried l t) => Eq (Application 'Curried l t)
deriving instance Ord (Term 'Curried l t) => Ord (Application 'Curried l t)

deriving instance Show (Term 'Uncurried l t) => Show (Application 'Uncurried l t)
deriving instance Read (Term 'Uncurried l t) => Read (Application 'Uncurried l t)
deriving instance Eq (Term 'Uncurried l t) => Eq (Application 'Uncurried l t)
deriving instance Ord (Term 'Uncurried l t) => Ord (Application 'Uncurried l t)

type Function :: Currying -> LambdaLifting -> Typing -> K.Type
data family Function

data instance Function 'Curried 'LambdaUnlifted t =
  FunctionC Identifier (Type 'Curried) (Term 'Curried 'LambdaUnlifted t)
  deriving Generic

data instance Function 'Uncurried 'LambdaUnlifted t =
  FunctionN [(Identifier, Type 'Uncurried)] (Term 'Uncurried 'LambdaUnlifted t)
  deriving Generic

deriving instance Show (Term 'Curried 'LambdaUnlifted t) => Show (Function 'Curried 'LambdaUnlifted t)
deriving instance Read (Term 'Curried 'LambdaUnlifted t) => Read (Function 'Curried 'LambdaUnlifted t)
deriving instance Eq (Term 'Curried 'LambdaUnlifted t) => Eq (Function 'Curried 'LambdaUnlifted t)
deriving instance Ord (Term 'Curried 'LambdaUnlifted t) => Ord (Function 'Curried 'LambdaUnlifted t)

deriving instance Show (Term 'Uncurried 'LambdaUnlifted t) => Show (Function 'Uncurried 'LambdaUnlifted t)
deriving instance Read (Term 'Uncurried 'LambdaUnlifted t) => Read (Function 'Uncurried 'LambdaUnlifted t)
deriving instance Eq (Term 'Uncurried 'LambdaUnlifted t) => Eq (Function 'Uncurried 'LambdaUnlifted t)
deriving instance Ord (Term 'Uncurried 'LambdaUnlifted t) => Ord (Function 'Uncurried 'LambdaUnlifted t)

data instance Function c 'LambdaLifted t deriving Show
