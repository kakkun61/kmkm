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

module Language.Kmkm.Syntax
  ( -- * Modules and definitions
    Module (..)
  , Definition (..)
  , CDefinition (..)
  , CHeader (..)
  , ValueBind (..)
    -- * Types
  , Type (..)
  , FunctionType (..)
    -- * Values
  , Term (..)
  , Term' (..)
  , ProcedureStep (..)
  , Literal (..)
  , Application (..)
  , Function (..)
  , TypeAnnotation (..)
    -- * Identifiers
  , Identifier (..)
  , QualifiedIdentifier (..)
  , ModuleName (..)
    -- * Kinds and types
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  ) where

import           Data.Function         (on)
import qualified Data.Kind             as K
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as N
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import qualified Language.C.Pretty     as C
import           Language.C.Syntax.AST (CExtDecl)
import qualified Text.PrettyPrint      as P

data Module c l t =
  Module ModuleName [ModuleName] [Definition c l t]
  deriving Generic

deriving instance Show (Definition c l t) => Show (Module c l t)
deriving instance Read (Definition c l t) => Read (Module c l t)
deriving instance Eq (Definition c l t) => Eq (Module c l t)
deriving instance Ord (Definition c l t) => Ord (Module c l t)

data Definition c l t
  = DataDefinition Identifier [(Identifier, [(Identifier, Type c)])]
  | TypeBind Identifier (Type c)
  | ValueBind (ValueBind c l t)
  | ForeignValueBind Identifier [CHeader] CDefinition (Type c)
  deriving Generic

deriving instance (Show (Term c l t), Show (Type c), Show (ValueBind c l t)) => Show (Definition c l t)
deriving instance (Eq (Term c l t), Eq (Type c), Eq (ValueBind c l t)) => Eq (Definition c l t)
deriving instance (Ord (Term c l t), Ord (Type c), Ord (ValueBind c l t)) => Ord (Definition c l t)

type ValueBind :: Currying -> LambdaLifting -> Typing -> K.Type
data family ValueBind

data instance ValueBind c 'LambdaUnlifted t =
  BindU Identifier (Term c 'LambdaUnlifted t)
  deriving Generic

data instance ValueBind c 'LambdaLifted t
  = BindV Identifier (Term c 'LambdaLifted t)
  | BindN Identifier [(Identifier, Type c)] (Term c 'LambdaLifted t)

deriving instance (Show (Term c 'LambdaUnlifted t), Show (Type c)) => Show (ValueBind c 'LambdaUnlifted t)
deriving instance (Read (Term c 'LambdaUnlifted t), Read (Type c)) => Read (ValueBind c 'LambdaUnlifted t)
deriving instance (Eq (Term c 'LambdaUnlifted t), Eq (Type c)) => Eq (ValueBind c 'LambdaUnlifted t)
deriving instance (Ord (Term c 'LambdaUnlifted t), Ord (Type c)) => Ord (ValueBind c 'LambdaUnlifted t)

deriving instance (Show (Term c 'LambdaLifted t), Show (Type c)) => Show (ValueBind c 'LambdaLifted t)
deriving instance (Read (Term c 'LambdaLifted t), Read (Type c)) => Read (ValueBind c 'LambdaLifted t)
deriving instance (Eq (Term c 'LambdaLifted t), Eq (Type c)) => Eq (ValueBind c 'LambdaLifted t)
deriving instance (Ord (Term c 'LambdaLifted t), Ord (Type c)) => Ord (ValueBind c 'LambdaLifted t)

newtype CDefinition = CDefinition CExtDecl deriving (Show, Generic)

instance Eq CDefinition where
  (==) = (==) `on` (\(CDefinition c) -> P.render $ C.pretty c)

instance Ord CDefinition where
  compare = compare `on` (\(CDefinition c) -> P.render $ C.pretty c)

data CHeader
  = SystemHeader Text
  | LocalHeader Text
  deriving (Show, Read, Eq, Ord, Generic)

type Type :: Currying -> K.Type
data Type c
  = TypeVariable Identifier
  | TypeApplication (Type c) (Type c)
  | FunctionType (FunctionType c)
  | ProcedureType (Type c)
  deriving Generic

deriving instance Show (FunctionType c) => Show (Type c)
deriving instance Read (FunctionType c) => Read (Type c)
deriving instance Eq (FunctionType c) => Eq (Type c)
deriving instance Ord (FunctionType c) => Ord (Type c)

type FunctionType :: Currying -> K.Type
data family FunctionType c

data instance FunctionType 'Curried =
  FunctionTypeC (Type 'Curried) (Type 'Curried)
  deriving (Show, Read, Eq, Ord, Generic)

data instance FunctionType 'Uncurried =
  FunctionTypeN [Type 'Uncurried] (Type 'Uncurried)
  deriving (Show, Read, Eq, Ord, Generic)

type Term :: Currying -> LambdaLifting -> Typing -> K.Type
data family Term

data instance Term c l 'Typed =
  TypedTerm (Term' c l 'Typed) (Type c)
  deriving Generic

newtype instance Term c l 'Untyped =
  UntypedTerm (Term' c l 'Untyped)
  deriving Generic

deriving instance (Show (Function 'Curried l 'Typed), Show (TypeAnnotation 'Curried l 'Typed), Show (ValueBind 'Curried l 'Typed)) => Show (Term 'Curried l 'Typed)
deriving instance (Read (Function 'Curried l 'Typed), Read (TypeAnnotation 'Curried l 'Typed), Read (ValueBind 'Curried l 'Typed), Read (Definition 'Curried l 'Typed)) => Read (Term 'Curried l 'Typed)
deriving instance (Eq (Function 'Curried l 'Typed), Eq (TypeAnnotation 'Curried l 'Typed), Eq (ValueBind 'Curried l 'Typed)) => Eq (Term 'Curried l 'Typed)
deriving instance (Ord (Function 'Curried l 'Typed), Ord (TypeAnnotation 'Curried l 'Typed), Ord (ValueBind 'Curried l 'Typed)) => Ord (Term 'Curried l 'Typed)

deriving instance (Show (Function 'Curried l 'Untyped), Show (ValueBind 'Curried l 'Untyped)) => Show (Term 'Curried l 'Untyped)
deriving instance (Read (Function 'Curried l 'Untyped), Read (ValueBind 'Curried l 'Untyped), Read (Definition 'Curried l 'Untyped)) => Read (Term 'Curried l 'Untyped)
deriving instance (Eq (Function 'Curried l 'Untyped), Eq (ValueBind 'Curried l 'Untyped)) => Eq (Term 'Curried l 'Untyped)
deriving instance (Ord (Function 'Curried l 'Untyped), Ord (ValueBind 'Curried l 'Untyped)) => Ord (Term 'Curried l 'Untyped)

deriving instance (Show (Function 'Uncurried l 'Typed), Show (TypeAnnotation 'Uncurried l 'Typed), Show (ValueBind 'Uncurried l 'Typed)) => Show (Term 'Uncurried l 'Typed)
deriving instance (Read (Function 'Uncurried l 'Typed), Read (TypeAnnotation 'Uncurried l 'Typed), Read (ValueBind 'Uncurried l 'Typed), Read (Definition 'Uncurried l 'Typed)) => Read (Term 'Uncurried l 'Typed)
deriving instance (Eq (Function 'Uncurried l 'Typed), Eq (TypeAnnotation 'Uncurried l 'Typed), Eq (ValueBind 'Uncurried l 'Typed)) => Eq (Term 'Uncurried l 'Typed)
deriving instance (Ord (Function 'Uncurried l 'Typed), Ord (TypeAnnotation 'Uncurried l 'Typed), Ord (ValueBind 'Uncurried l 'Typed)) => Ord (Term 'Uncurried l 'Typed)

deriving instance (Show (Function 'Uncurried l 'Untyped), Show (ValueBind 'Uncurried l 'Untyped)) => Show (Term 'Uncurried l 'Untyped)
deriving instance (Read (Function 'Uncurried l 'Untyped), Read (ValueBind 'Uncurried l 'Untyped), Read (Definition 'Uncurried l 'Untyped)) => Read (Term 'Uncurried l 'Untyped)
deriving instance (Eq (Function 'Uncurried l 'Untyped), Eq (ValueBind 'Uncurried l 'Untyped)) => Eq (Term 'Uncurried l 'Untyped)
deriving instance (Ord (Function 'Uncurried l 'Untyped), Ord (ValueBind 'Uncurried l 'Untyped)) => Ord (Term 'Uncurried l 'Untyped)

data Term' c l t
  = Variable QualifiedIdentifier
  | Literal (Literal c l t)
  | Application (Application c l t)
  | Procedure (NonEmpty (ProcedureStep c l t))
  | TypeAnnotation (TypeAnnotation c l t)
  | Let [Definition c l t] (Term c l t)
  deriving Generic

deriving instance (Show (Application c l t), Show (Literal c l t), Show (ProcedureStep c l t), Show (TypeAnnotation c l t), Show (ValueBind c l t), Show (Term c l t), Show (FunctionType c)) => Show (Term' c l t)
deriving instance (Read (Application c l t), Read (Literal c l t), Read (ProcedureStep c l t), Read (TypeAnnotation c l t), Read (ValueBind c l t), Read (Term c l t), Read (FunctionType c), Read (Definition c l t)) => Read (Term' c l t)
deriving instance (Eq (Application c l t), Eq (Literal c l t), Eq (ProcedureStep c l t), Eq (TypeAnnotation c l t), Eq (ValueBind c l t), Eq (Term c l t), Eq (FunctionType c)) => Eq (Term' c l t)
deriving instance (Ord (Application c l t), Ord (Literal c l t), Ord (ProcedureStep c l t), Ord (TypeAnnotation c l t), Ord (ValueBind c l t), Ord (Term c l t), Ord (FunctionType c)) => Ord (Term' c l t)

type TypeAnnotation :: Currying -> LambdaLifting -> Typing -> K.Type
data family TypeAnnotation c l t

data instance TypeAnnotation c l 'Untyped =
  TypeAnnotation' (Term c l 'Untyped) (Type c)

data instance TypeAnnotation c l 'Typed

deriving instance (Show (FunctionType c), Show (Term c l 'Untyped)) => Show (TypeAnnotation c l 'Untyped)
deriving instance (Read (FunctionType c), Read (Term c l 'Untyped)) => Read (TypeAnnotation c l 'Untyped)
deriving instance (Eq (FunctionType c), Eq (Term c l 'Untyped)) => Eq (TypeAnnotation c l 'Untyped)
deriving instance (Ord (FunctionType c), Ord (Term c l 'Untyped)) => Ord (TypeAnnotation c l 'Untyped)

deriving instance (Show (FunctionType c), Show (Term c l 'Untyped)) => Show (TypeAnnotation c l 'Typed)
deriving instance (Read (FunctionType c), Read (Term c l 'Untyped)) => Read (TypeAnnotation c l 'Typed)
deriving instance (Eq (FunctionType c), Eq (Term c l 'Untyped)) => Eq (TypeAnnotation c l 'Typed)
deriving instance (Ord (FunctionType c), Ord (Term c l 'Untyped)) => Ord (TypeAnnotation c l 'Typed)

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

data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = UserIdentifier . fromString

data QualifiedIdentifier =
  QualifiedIdentifier (Maybe ModuleName) Identifier
  deriving (Show, Read, Eq, Ord, Generic)

newtype ModuleName = ModuleName (N.NonEmpty Text) deriving (Show, Read, Eq, Ord, Generic)

instance IsString ModuleName where
  fromString = ModuleName . (:| []) . fromString

data Currying
  = Curried
  | Uncurried
  deriving (Show, Read, Eq, Ord, Generic)

data Typing
 = Typed
 | Untyped
  deriving (Show, Read, Eq, Ord, Generic)

data LambdaLifting
  = LambdaLifted
  | LambdaUnlifted
  deriving (Show, Read, Eq, Ord, Generic)
