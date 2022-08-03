{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE EmptyDataDeriving        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE PatternSynonyms #-}

#if __GLASGOW_HASKELL__ < 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm.Internal.Syntax
  ( -- * Modules and definitions
    Module (..)
  , Definition (..)
  , ValueConstructor (..)
  , Field (..)
  , ValueBind (..)
    -- * Types
  , Type (..)
  , FunctionType (..)
    -- * Values
  , Value (..)
  , Value' (..)
  , ProcedureStep (..)
  , Literal (..)
  , Application (..)
  , Function (..)
  , TypeAnnotation (..)
    -- * Identifiers
  , Identifier (..)
  , QualifiedIdentifier (..)
  , EitherIdentifier (..)
  , BindIdentifier
  , ReferenceIdentifier
  , ModuleName (..)
    -- * Embedded
  , Target (..)
  , EmbeddedValue (..)
  , EmbeddedCValue (..)
  , isEmbeddedCValue
  , EmbeddedType (..)
  , EmbeddedCType (..)
  , isEmbeddedCType
    -- * Locations
  , Location (..)
  , Position (..)
    -- * Kinds and types
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  , NameResolving (..)
    -- * Higher kinded data
  , toIdentity
    -- * Pretty printing
  , Pretty (..)
  ) where

import qualified Language.Kmkm.Internal.Exception as X

import           Data.Bifunctor              (Bifunctor (bimap))
import           Data.Copointed              (Copointed (copoint))
import           Data.Foldable               (Foldable (fold))
import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Identity       (Identity (Identity))
import qualified Data.Kind                   as K
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import qualified Data.List.NonEmpty          as N
import           Data.Pointed                (Pointed (point))
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Exts                    (IsList)
import qualified GHC.Exts                    as E
import           GHC.Generics                (Generic)
import Data.Functor.With ( With, pattern With )

-- Module

type Module :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data Module n c l t et ev f =
  Module (f ModuleName) (f [f ModuleName]) (f [f (Definition n c l t et ev f)])
  deriving Generic

type ModuleConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type ModuleConstraint cls n c l t et ev f =
  ( cls (f ModuleName)
  , cls (f [f ModuleName])
  , cls (f (Definition n c l t et ev f))
  , cls (f [f (Definition n c l t et ev f)])
  )

deriving instance ModuleConstraint Show n c l t et ev f => Show (Module n c l t et ev f)
deriving instance ModuleConstraint Eq n c l t et ev f => Eq (Module n c l t et ev f)
deriving instance ModuleConstraint Ord n c l t et ev f => Ord (Module n c l t et ev f)

instance
  ( FunctorB (FunctionType n c)
  , FunctorB (ValueBind n c l t et ev)
  , FunctorB et
  , FunctorB ev
  ) =>
  FunctorB (Module n c l t et ev) where
  bmap f (Module n ms ds) = Module (f n) (fmap f <$> f ms) (fmap (fmap (bmap f) . f) <$> f ds)

-- Definition

type Definition :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data Definition n c l t et ev f
  = DataDefinition (f (BindIdentifier n)) (f [f (ValueConstructor n c l et ev f)])
  | TypeBind (f (BindIdentifier n)) (f (Type n c f))
  | ValueBind (f (ValueBind n c l t et ev f))
  | ForeignTypeBind (f (BindIdentifier n)) (f (et f))
  | ForeignValueBind (f (BindIdentifier n)) (f (ev f)) (f (Type n c f))
  deriving Generic

type DefinitionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type DefinitionConstraint cls n c l t et ev f =
  ( cls (f (BindIdentifier n))
  , cls (f [f (ValueConstructor n c l et ev f)])
  , cls (f (Type n c f))
  , cls (f (ValueBind n c l t et ev f))
  , cls (f CHeader)
  , cls (f [f CHeader])
  , cls (f (et f))
  , cls (f (ev f))
  )

deriving instance DefinitionConstraint Show n c l t et ev f => Show (Definition n c l t et ev f)
deriving instance DefinitionConstraint Eq n c l t et ev f => Eq (Definition n c l t et ev f)
deriving instance DefinitionConstraint Ord n c l t et ev f => Ord (Definition n c l t et ev f)

instance (FunctorB (Type n c), FunctorB (FunctionType n c), FunctorB (ValueBind n c l t et ev), FunctorB et, FunctorB ev) => FunctorB (Definition n c l t et ev) where
  bmap :: forall f g. (Functor f, Functor g) => (forall a. f a -> g a) -> Definition n c l t et ev f -> Definition n c l t et ev g
  bmap f (DataDefinition n cs) =
    DataDefinition (f n) $ fmap (fmap (bmap f) . f) <$> f cs
  bmap f (TypeBind i t) = TypeBind (f i) (bmap f <$> f t)
  bmap f (ValueBind b) = ValueBind (bmap f <$> f b)
  bmap f (ForeignTypeBind i c) =  ForeignTypeBind (f i) (bmap f <$> f c)
  bmap f (ForeignValueBind i c t) = ForeignValueBind (f i) (bmap f <$> f c) (bmap f <$> f t)

-- ValueConstructor

type ValueConstructor :: NameResolving -> Currying -> LambdaLifting -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data ValueConstructor n c l et ev f = ValueConstructor (f (BindIdentifier n)) (f [f (Field n c l et ev f)])

type ValueConstructorConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type ValueConstructorConstraint cls n c l et ev f =
  ( cls (f (BindIdentifier n))
  , cls (f [f (Field n c l et ev f)])
  )

deriving instance ValueConstructorConstraint Show n c l et ev f => Show (ValueConstructor n c l et ev f)
deriving instance ValueConstructorConstraint Eq n c l et ev f => Eq (ValueConstructor n c l et ev f)
deriving instance ValueConstructorConstraint Ord n c l et ev f => Ord (ValueConstructor n c l et ev f)

instance FunctorB (FunctionType n c) => FunctorB (ValueConstructor n c l et ev) where
  bmap f (ValueConstructor i fs) = ValueConstructor (f i) (fmap (fmap (bmap f) . f) <$> f fs)

-- Field

type Field :: NameResolving -> Currying -> LambdaLifting -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data Field n c l et ev f = Field (f (BindIdentifier n)) (f (Type n c f))

type FieldConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type FieldConstraint cls n c l et ev f =
  ( cls (f (BindIdentifier n))
  , cls (f (Type n c f))
  )

deriving instance FieldConstraint Show n c l et ev f => Show (Field n c l et ev f)
deriving instance FieldConstraint Eq n c l et ev f => Eq (Field n c l et ev f)
deriving instance FieldConstraint Ord n c l et ev f => Ord (Field n c l et ev f)

instance FunctorB (FunctionType n c) => FunctorB (Field n c l et ev) where
  bmap f (Field i t) = Field (f i) (bmap f <$> f t)

-- ValueBind

type ValueBind :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data family ValueBind

type ValueBindConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type family ValueBindConstraint cls n c l t et ev f where
  ValueBindConstraint cls n c 'LambdaUnlifted t et ev f =
    ( cls (f (BindIdentifier n))
    , cls (f (Value n c 'LambdaUnlifted t et ev f))
    )

  ValueBindConstraint cls n c 'LambdaLifted t et ev f =
    ( cls (f (BindIdentifier n))
    , cls (f (Value n c 'LambdaLifted t et ev f))
    , cls (f (Type n c f))
    , cls (f [f (f (BindIdentifier n), f (Type n c f))])
    )

data instance ValueBind n c 'LambdaUnlifted t et ev f =
  ValueBindU (f (BindIdentifier n)) (f (Value n c 'LambdaUnlifted t et ev f))
  deriving Generic

deriving instance ValueBindConstraint Show n c 'LambdaUnlifted t et ev f => Show (ValueBind n c 'LambdaUnlifted t et ev f)
deriving instance ValueBindConstraint Eq n c 'LambdaUnlifted t et ev f => Eq (ValueBind n c 'LambdaUnlifted t et ev f)
deriving instance ValueBindConstraint Ord n c 'LambdaUnlifted t et ev f => Ord (ValueBind n c 'LambdaUnlifted t et ev f)

instance FunctorB (Value n c 'LambdaUnlifted t et ev) => FunctorB (ValueBind n c 'LambdaUnlifted t et ev) where
  bmap f (ValueBindU i v) = ValueBindU (f i) (bmap f <$> f v)

data instance ValueBind n c 'LambdaLifted t et ev f
  = ValueBindV (f (BindIdentifier n)) (f (Value n c 'LambdaLifted t et ev f))
  | ValueBindN (f (BindIdentifier n)) (f [f (f (BindIdentifier n), f (Type n c f))]) (f (Value n c 'LambdaLifted t et ev f))
  deriving Generic

deriving instance ValueBindConstraint Show n c 'LambdaLifted t et ev f => Show (ValueBind n c 'LambdaLifted t et ev f)
deriving instance ValueBindConstraint Eq n c 'LambdaLifted t et ev f => Eq (ValueBind n c 'LambdaLifted t et ev f)
deriving instance ValueBindConstraint Ord n c 'LambdaLifted t et ev f => Ord (ValueBind n c 'LambdaLifted t et ev f)

instance (FunctorB (Value n c 'LambdaLifted t et ev), FunctorB (FunctionType n c)) => FunctorB (ValueBind n c 'LambdaLifted t et ev) where
  bmap f (ValueBindV i v) = ValueBindV (f i) (bmap f <$> f v)
  bmap f (ValueBindN i ps v) = ValueBindN (f i) (fmap (fmap (bimap f (fmap (bmap f) . f)) . f) <$> f ps) (bmap f <$> f v)

-- EmbeddedValue

newtype EmbeddedValue f
  = EmbeddedValueC (EmbeddedCValue f)
  deriving Generic

deriving instance Show (EmbeddedCValue f) => Show (EmbeddedValue f)
deriving instance Read (EmbeddedCValue f) => Read (EmbeddedValue f)
deriving instance Eq (EmbeddedCValue f) => Eq (EmbeddedValue f)
deriving instance Ord (EmbeddedCValue f) => Ord (EmbeddedValue f)

instance FunctorB EmbeddedValue where
  bmap f (EmbeddedValueC v) = EmbeddedValueC (bmap f v)

-- EmbeddedCValue

data EmbeddedCValue f =
  EmbeddedCValue { include :: f Text, parameters :: f [f Text], body :: f Text }
  deriving Generic

deriving instance (Show (f Text), Show (f [f Text])) => Show (EmbeddedCValue f)
deriving instance (Read (f Text), Read (f [f Text])) => Read (EmbeddedCValue f)
deriving instance (Eq (f Text), Eq (f [f Text])) => Eq (EmbeddedCValue f)
deriving instance (Ord (f Text), Ord (f [f Text])) => Ord (EmbeddedCValue f)

instance FunctorB EmbeddedCValue where
  bmap f (EmbeddedCValue i ps b) = EmbeddedCValue (f i) (fmap f <$> f ps) (f b)

isEmbeddedCValue :: Traversable f => f (EmbeddedValue f) -> Maybe (f (EmbeddedCValue f))
isEmbeddedCValue = traverse $ \(EmbeddedValueC v) -> Just v

-- EmbeddedType

newtype EmbeddedType f
  = EmbeddedTypeC (EmbeddedCType f)
  deriving Generic

deriving instance (Show (f Text), Show (f [f Text])) => Show (EmbeddedType f)
deriving instance (Read (f Text), Read (f [f Text])) => Read (EmbeddedType f)
deriving instance (Eq (f Text), Eq (f [f Text])) => Eq (EmbeddedType f)
deriving instance (Ord (f Text), Ord (f [f Text])) => Ord (EmbeddedType f)

instance FunctorB EmbeddedType where
  bmap f (EmbeddedTypeC t) = EmbeddedTypeC (bmap f t)

-- EmbeddedCType

data EmbeddedCType f =
  EmbeddedCType { include :: f Text, body :: f Text }
  deriving Generic

deriving instance (Show (f Text), Show (f [f Text])) => Show (EmbeddedCType f)
deriving instance (Read (f Text), Read (f [f Text])) => Read (EmbeddedCType f)
deriving instance (Eq (f Text), Eq (f [f Text])) => Eq (EmbeddedCType f)
deriving instance (Ord (f Text), Ord (f [f Text])) => Ord (EmbeddedCType f)

instance FunctorB EmbeddedCType where
  bmap f (EmbeddedCType i b) = EmbeddedCType (f i) (f b)

isEmbeddedCType :: Traversable f => f (EmbeddedType f) -> Maybe (f (EmbeddedCType f))
isEmbeddedCType = traverse $ \(EmbeddedTypeC v) -> Just v

-- CHeader

data CHeader
  = SystemHeader Text
  | LocalHeader Text
  deriving (Show, Read, Eq, Ord, Generic)

-- Type

type Type :: NameResolving -> Currying-> (K.Type -> K.Type) -> K.Type
data Type n c f
  = TypeVariable (f (ReferenceIdentifier n))
  | TypeApplication (f (Type n c f)) (f (Type n c f))
  | FunctionType (f (FunctionType n c f))
  | ProcedureType (f (Type n c f))
  | ForAllType (f (BindIdentifier n)) (f (Type n c f))
  deriving Generic

type TypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying-> (K.Type -> K.Type) -> K.Constraint
type TypeConstraint cls n c f =
  ( cls (f (ReferenceIdentifier n))
  , cls (FunctionType n c f)
  , cls (f (Type n c f))
  , cls (f (FunctionType n c f))
  , cls (f (BindIdentifier n))
  )

deriving instance TypeConstraint Show n c f => Show (Type n c f)
deriving instance TypeConstraint Read n c f => Read (Type n c f)
deriving instance TypeConstraint Eq n c f => Eq (Type n c f)
deriving instance TypeConstraint Ord n c f => Ord (Type n c f)

instance FunctorB (FunctionType n c) => FunctorB (Type n c) where
  bmap f (TypeVariable i)        = TypeVariable (f i)
  bmap f (TypeApplication t1 t2) = TypeApplication (bmap f <$> f t1) (bmap f <$> f t2)
  bmap f (FunctionType g)        = FunctionType (bmap f <$> f g)
  bmap f (ProcedureType t)       = ProcedureType (bmap f <$> f t)
  bmap f (ForAllType i t)        = ForAllType (f i) (bmap f <$> f t)

instance
  ( Pretty (f (Type n c f))
  , Pretty (f (ReferenceIdentifier n))
  , Pretty (f (FunctionType n c f))
  , Pretty (f (BindIdentifier n))
  ) =>
  Pretty (Type n c f) where

  pretty (TypeVariable i)        = pretty i
  pretty (TypeApplication t1 t2) = "(apply " <> pretty t1 <> " " <> pretty t2 <> ")"
  pretty (FunctionType f)        = pretty f
  pretty (ProcedureType t)       = "(procedure " <> pretty t <> ")"
  pretty (ForAllType i t)        = "(for-all " <> pretty i <> " " <> pretty t <> ")"

instance (IsString (ReferenceIdentifier n), Pointed f) => IsString (Type n c f) where
  fromString = TypeVariable . point . fromString

instance (IsList (ReferenceIdentifier n), E.Item (ReferenceIdentifier n) ~ Text, Pointed f, Copointed f) => IsList (Type n c f) where
  type Item (Type n c f) = Text
  fromList = TypeVariable . point . E.fromList
  toList (TypeVariable v) = E.toList $ copoint v
  toList _                = error "only type variable acceptable"

-- FunctionType

type FunctionType :: NameResolving -> Currying-> (K.Type -> K.Type) -> K.Type
data family FunctionType

type FunctionTypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying-> (K.Type -> K.Type) -> K.Constraint
type family FunctionTypeConstraint cls n c f where
  FunctionTypeConstraint cls n 'Curried f =
    ( cls (f (ReferenceIdentifier n))
    , cls (f (Type n 'Curried f))
    )

  FunctionTypeConstraint cls n 'Uncurried f =
    ( cls (f (ReferenceIdentifier n))
    , cls (f (Type n 'Uncurried f))
    , cls (f [f (Type n 'Uncurried f)])
    )

data instance FunctionType n 'Curried f =
  FunctionTypeC (f (Type n 'Curried f)) (f (Type n 'Curried f))
  deriving Generic

deriving instance FunctionTypeConstraint Show n 'Curried f => Show (FunctionType n 'Curried f)
deriving instance FunctionTypeConstraint Read n 'Curried f => Read (FunctionType n 'Curried f)
deriving instance FunctionTypeConstraint Eq n 'Curried f => Eq (FunctionType n 'Curried f)
deriving instance FunctionTypeConstraint Ord n 'Curried f => Ord (FunctionType n 'Curried f)

instance FunctorB (FunctionType n 'Curried) where
  bmap f (FunctionTypeC t1 t2) = FunctionTypeC (bmap f <$> f t1) (bmap f <$> f t2)

instance Pretty (f (Type n 'Curried f)) => Pretty (FunctionType n 'Curried f) where
  pretty (FunctionTypeC t1 t2) = "(function " <> pretty t1 <> " " <> pretty t2 <> ")"

data instance FunctionType n 'Uncurried f =
  FunctionTypeN (f [f (Type n 'Uncurried f)]) (f (Type n 'Uncurried f))
  deriving Generic

deriving instance FunctionTypeConstraint Show n 'Uncurried f => Show (FunctionType n 'Uncurried f)
deriving instance FunctionTypeConstraint Read n 'Uncurried f => Read (FunctionType n 'Uncurried f)
deriving instance FunctionTypeConstraint Eq n 'Uncurried f => Eq (FunctionType n 'Uncurried f)
deriving instance FunctionTypeConstraint Ord n 'Uncurried f => Ord (FunctionType n 'Uncurried f)

instance FunctorB (FunctionType n 'Uncurried) where
  bmap f (FunctionTypeN ts t) = FunctionTypeN (fmap (fmap (bmap f) . f) <$> f ts) (bmap f <$> f t)

instance
  ( Pretty (f [f (Type n 'Uncurried f)])
  , Pretty (f (Type n 'Uncurried f))
  ) =>
  Pretty (FunctionType n 'Uncurried f) where

  pretty (FunctionTypeN ts t) = "(function " <> pretty ts <> " " <> pretty t <> ")"

-- Value

type Value :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data family Value

type ValueConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type family ValueConstraint cls n c l t et ev f where
  ValueConstraint cls n c l 'Untyped et ev f =
    ( cls (f (Value' n c l 'Untyped et ev f))
    , cls (f (ReferenceIdentifier n))
    , cls (f (BindIdentifier n))
    , cls (f Literal)
    )

  ValueConstraint cls n c l 'Typed et ev f =
    ( cls (f (Value' n c l 'Typed et ev f))
    , cls (f (ReferenceIdentifier n))
    , cls (f (BindIdentifier n))
    , cls (f Literal)
    , cls (f (Type n c f))
    )

newtype instance Value n c l 'Untyped et ev f =
  UntypedValue (f (Value' n c l 'Untyped et ev f))
  deriving Generic

deriving instance ValueConstraint Show n c l 'Untyped et ev f => Show (Value n c l 'Untyped et ev f)
deriving instance ValueConstraint Eq n c l 'Untyped et ev f => Eq (Value n c l 'Untyped et ev f)
deriving instance ValueConstraint Ord n c l 'Untyped et ev f => Ord (Value n c l 'Untyped et ev f)

instance
  ( FunctorB (Function n c l 'Untyped et ev)
  , FunctorB (Application n c l 'Untyped et ev)
  , FunctorB (FunctionType n c)
  , FunctorB (ValueBind n c l 'Untyped et ev)
  , FunctorB et
  , FunctorB ev
  ) =>
  FunctorB (Value n c l 'Untyped et ev) where

  bmap f (UntypedValue v) = UntypedValue (bmap f <$> f v)

data instance Value n c l 'Typed et ev f =
  TypedValue (f (Value' n c l 'Typed et ev f)) (f (Type n c f))
  deriving Generic

deriving instance ValueConstraint Show n c l 'Typed et ev f => Show (Value n c l 'Typed et ev f)
deriving instance ValueConstraint Eq n c l 'Typed et ev f => Eq (Value n c l 'Typed et ev f)
deriving instance ValueConstraint Ord n c l 'Typed et ev f => Ord (Value n c l 'Typed et ev f)

instance
  ( FunctorB (Function n c l 'Typed et ev)
  , FunctorB (Application n c l 'Typed et ev)
  , FunctorB (FunctionType n c)
  , FunctorB (ValueBind n c l 'Typed et ev)
  , FunctorB et
  , FunctorB ev
  ) =>
  FunctorB (Value n c l 'Typed et ev) where

  bmap f (TypedValue v t) = TypedValue (bmap f <$> f v) (bmap f <$> f t)

instance (IsString (ReferenceIdentifier n), Pointed f) => IsString (Value n c l 'Untyped et ev f) where
  fromString = UntypedValue . point . fromString

instance (IsList (ReferenceIdentifier n), E.Item (ReferenceIdentifier n) ~ Text, Pointed f, Copointed f) => IsList (Value n c l 'Untyped et ev f) where
  type Item (Value n c l 'Untyped et ev f) = Text
  fromList = UntypedValue . point . E.fromList
  toList (UntypedValue v)
    | Variable i <- copoint v = E.toList $ copoint i
  toList _ = error "only variable acceptable"

-- Value'

data Value' n c l t et ev f
  = Variable (f (ReferenceIdentifier n))
  | Literal (f Literal)
  | Function (f (Function n c l t et ev f))
  | Application (f (Application n c l t et ev f))
  | Procedure (f (NonEmpty (f (ProcedureStep n c l t et ev f))))
  | TypeAnnotation (f (TypeAnnotation n c l t et ev f))
  | Let (f [f (Definition n c l t et ev f)]) (f (Value n c l t et ev f))
  | ForAll (f (BindIdentifier n)) (f (Value n c l t et ev f))
  deriving Generic

type Value'Constraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type Value'Constraint cls n c l t et ev f =
  ( cls (f (ReferenceIdentifier n))
  , cls (f (Function n c l t et ev f))
  , cls (f (BindIdentifier n))
  , cls (f (Application n c l t et ev f))
  , cls (f (ProcedureStep n c l t et ev f))
  , cls (f (NonEmpty (f (ProcedureStep n c l t et ev f))))
  , cls (f (TypeAnnotation n c l t et ev f))
  , cls (f (ValueBind n c l t et ev f))
  , cls (f (Value n c l t et ev f))
  , cls (f (Type n c f))
  , cls (f (Definition n c l t et ev f))
  , cls (f [f (Definition n c l t et ev f)])
  , cls (f (ProcedureStep n c l t et ev f))
  , cls (f Literal)
  , cls (Function n c l t et ev f)
  )

deriving instance Value'Constraint Show n c l t et ev f => Show (Value' n c l t et ev f)
deriving instance Value'Constraint Eq n c l t et ev f => Eq (Value' n c l t et ev f)
deriving instance Value'Constraint Ord n c l t et ev f => Ord (Value' n c l t et ev f)

instance
  ( FunctorB (Function n c l t et ev)
  , FunctorB (Application n c l t et ev)
  , FunctorB (TypeAnnotation n c l t et ev)
  , FunctorB (Value n c l t et ev)
  , FunctorB (FunctionType n c)
  , FunctorB (ValueBind n c l t et ev)
  , FunctorB et
  , FunctorB ev
  ) =>
  FunctorB (Value' n c l t et ev) where

  bmap f (Variable i)       = Variable $ f i
  bmap f (Literal l)        = Literal $ f l
  bmap f (Function v)       = Function $ bmap f <$> f v
  bmap f (Application a)    = Application $ bmap f <$> f a
  bmap f (Procedure ps)     = Procedure $ fmap (fmap (bmap f) . f) <$> f ps
  bmap f (TypeAnnotation a) = TypeAnnotation $ bmap f <$> f a
  bmap f (Let ds v)         = Let (fmap (fmap (bmap f) . f) <$> f ds) $ bmap f <$> f v
  bmap f (ForAll i v)       = ForAll (f i) $ bmap f <$> f v

instance (IsString (ReferenceIdentifier n), Pointed f) => IsString (Value' n c l t et ev f) where
  fromString = Variable . point . fromString

instance (IsList (ReferenceIdentifier n), E.Item (ReferenceIdentifier n) ~ Text, Pointed f, Copointed f) => IsList (Value' n c l t et ev f) where
  type Item (Value' n c l t et ev f) = Text
  fromList = Variable . point . E.fromList
  toList (Variable i) = E.toList $ copoint i
  toList _            = error "only variable acceptable"

-- TypeAnnotation

type TypeAnnotation :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data family TypeAnnotation

data instance TypeAnnotation n c l 'Untyped et ev f =
  TypeAnnotation' (f (Value n c l 'Untyped et ev f)) (f (Type n c f))
  deriving Generic

type TypeAnnotationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type TypeAnnotationConstraint cls n c l t et ev f =
  ( cls (f (ReferenceIdentifier n))
  , cls (f (BindIdentifier n))
  , cls (f (Function n c l t et ev f))
  , cls (f (Application n c l t et ev f))
  , cls (f (ProcedureStep n c l t et ev f))
  , cls (f (ValueBind n c l t et ev f))
  , cls (f (FunctionType n c f))
  , cls (f (Value n c l t et ev f))
  , cls (f (Type n c f))
  )

deriving instance TypeAnnotationConstraint Show n c l 'Untyped et ev f => Show (TypeAnnotation n c l 'Untyped et ev f)
deriving instance TypeAnnotationConstraint Eq n c l 'Untyped et ev f => Eq (TypeAnnotation n c l 'Untyped et ev f)
deriving instance TypeAnnotationConstraint Ord n c l 'Untyped et ev f => Ord (TypeAnnotation n c l 'Untyped et ev f)

instance
  ( FunctorB (Function n c l 'Untyped et ev)
  , FunctorB (Application n c l 'Untyped et ev)
  , FunctorB (FunctionType n c)
  , FunctorB (ValueBind n c l 'Untyped et ev)
  , FunctorB et
  , FunctorB ev
  ) =>
  FunctorB (TypeAnnotation n c l 'Untyped et ev) where

  bmap f (TypeAnnotation' v t) = TypeAnnotation' (bmap f <$> f v) (bmap f <$> f t)

data instance TypeAnnotation n c l 'Typed et ev f
  deriving Generic

deriving instance Show (TypeAnnotation n c l 'Typed et ev f)
deriving instance TypeAnnotationConstraint Eq n c l 'Typed et ev f => Eq (TypeAnnotation n c l 'Typed et ev f)
deriving instance TypeAnnotationConstraint Ord n c l 'Typed et ev f => Ord (TypeAnnotation n c l 'Typed et ev f)

instance FunctorB (TypeAnnotation n c l 'Typed et ev) where
  bmap _ = X.unreachable

-- ProcedureStep

data ProcedureStep n c l t et ev f
  = BindProcedureStep (f (BindIdentifier n)) (f (Value n c l t et ev f))
  | CallProcedureStep (f (Value n c l t et ev f))

type ProcedureStepConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type ProcedureStepConstraint cls n c l t et ev f =
  ( cls (f (BindIdentifier n))
  , cls (f (Value n c l t et ev f))
  )

deriving instance ProcedureStepConstraint Show n c l t et ev f => Show (ProcedureStep n c l t et ev f)
deriving instance ProcedureStepConstraint Eq n c l t et ev f => Eq (ProcedureStep n c l t et ev f)
deriving instance ProcedureStepConstraint Ord n c l t et ev f => Ord (ProcedureStep n c l t et ev f)

instance FunctorB (Value n c l t et ev) => FunctorB (ProcedureStep n c l t et ev) where
  bmap f (BindProcedureStep i v) = BindProcedureStep (f i) (bmap f <$> f v)
  bmap f (CallProcedureStep v)   = CallProcedureStep (bmap f <$> f v)

-- Literal

data Literal
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)

-- Application

type Application :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data family Application

type ApplicationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type family ApplicationConstraint cls n c l t et ev f where
  ApplicationConstraint cls n 'Curried l t et ev f =
    (cls (f (Value n 'Curried l t et ev f)))

  ApplicationConstraint cls n 'Uncurried l t et ev f =
    ( cls (f (Value n 'Uncurried l t et ev f))
    , cls (f [f (Value n 'Uncurried l t et ev f)])
    )

data instance Application n 'Curried l t et ev f =
  ApplicationC (f (Value n 'Curried l t et ev f)) (f (Value n 'Curried l t et ev f))
  deriving Generic

deriving instance ApplicationConstraint Show n 'Curried l t et ev f => Show (Application n 'Curried l t et ev f)
deriving instance ApplicationConstraint Eq n 'Curried l t et ev f => Eq (Application n 'Curried l t et ev f)
deriving instance ApplicationConstraint Ord n 'Curried l t et ev f => Ord (Application n 'Curried l t et ev f)

instance FunctorB (Value n 'Curried l t et ev) => FunctorB (Application n 'Curried l t et ev) where
  bmap f (ApplicationC v1 v2) = ApplicationC (bmap f <$> f v1) (bmap f <$> f v2)

data instance Application n 'Uncurried l t et ev f =
  ApplicationN (f (Value n 'Uncurried l t et ev f)) (f [f (Value n 'Uncurried l t et ev f)])
  deriving Generic

deriving instance ApplicationConstraint Show n 'Uncurried l t et ev f => Show (Application n 'Uncurried l t et ev f)
deriving instance ApplicationConstraint Eq n 'Uncurried l t et ev f => Eq (Application n 'Uncurried l t et ev f)
deriving instance ApplicationConstraint Ord n 'Uncurried l t et ev f => Ord (Application n 'Uncurried l t et ev f)

instance FunctorB (Value n 'Uncurried l t et ev) => FunctorB (Application n 'Uncurried l t et ev) where
  bmap f (ApplicationN v1 vs) = ApplicationN (bmap f <$> f v1) $ fmap (fmap (bmap f) . f) <$> f vs

-- Function

type Function :: NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Type
data family Function

type FunctionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type)-> (K.Type -> K.Type) -> K.Constraint
type family FunctionConstraint cls n c l t et ev f where
  FunctionConstraint cls n 'Curried l t et ev f =
    ( cls (f (BindIdentifier n))
    , cls (f (ReferenceIdentifier n))
    , cls (f (Value n 'Curried l t et ev f))
    , cls (f (Type n 'Curried f))
    )

  FunctionConstraint cls n 'Uncurried l t et ev f =
    ( cls (f (BindIdentifier n))
    , cls (f (ReferenceIdentifier n))
    , cls (f (Value n 'Uncurried l t et ev f))
    , cls (f (Type n 'Uncurried f))
    , cls (f [f (f (BindIdentifier n), f (Type n 'Uncurried f))])
    )

data instance Function n 'Curried 'LambdaUnlifted t et ev f =
  FunctionC (f (BindIdentifier n)) (f (Type n 'Curried f)) (f (Value n 'Curried 'LambdaUnlifted t et ev f))
  deriving Generic

deriving instance FunctionConstraint Show n 'Curried 'LambdaUnlifted t et ev f => Show (Function n 'Curried 'LambdaUnlifted t et ev f)
deriving instance FunctionConstraint Eq n 'Curried 'LambdaUnlifted t et ev f => Eq (Function n 'Curried 'LambdaUnlifted t et ev f)
deriving instance FunctionConstraint Ord n 'Curried 'LambdaUnlifted t et ev f => Ord (Function n 'Curried 'LambdaUnlifted t et ev f)

instance FunctorB (Value n 'Curried 'LambdaUnlifted t et ev) => FunctorB (Function n 'Curried 'LambdaUnlifted t et ev) where
  bmap f (FunctionC i t v) = FunctionC (f i) (bmap f <$> f t) (bmap f <$> f v)

data instance Function n 'Uncurried 'LambdaUnlifted t et ev f =
  FunctionN (f [f (f (BindIdentifier n), f (Type n 'Uncurried f))]) (f (Value n 'Uncurried 'LambdaUnlifted t et ev f))
  deriving Generic

deriving instance FunctionConstraint Show n 'Uncurried 'LambdaUnlifted t et ev f => Show (Function n 'Uncurried 'LambdaUnlifted t et ev f)
deriving instance FunctionConstraint Eq n 'Uncurried 'LambdaUnlifted t et ev f => Eq (Function n 'Uncurried 'LambdaUnlifted t et ev f)
deriving instance FunctionConstraint Ord n 'Uncurried 'LambdaUnlifted t et ev f => Ord (Function n 'Uncurried 'LambdaUnlifted t et ev f)

instance FunctorB (Value n 'Uncurried 'LambdaUnlifted t et ev) => FunctorB (Function n 'Uncurried 'LambdaUnlifted t et ev) where
  bmap f (FunctionN ps v) = FunctionN (fmap (fmap (bimap f (fmap (bmap f) . f)) . f) <$> f ps) (bmap f <$> f v)

data instance Function n c 'LambdaLifted t f et ev deriving (Show, Read, Eq, Ord, Generic)

instance FunctorB (Function n c 'LambdaLifted t et ev) where
  bmap _ = X.unreachable

-- Identifiers

-- | An identifier.
data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = UserIdentifier . fromString

instance Pretty Identifier where
  pretty (UserIdentifier t)     = t
  pretty (SystemIdentifier c n) = "_" <> T.pack [c] <> T.pack (show n)

-- | A qualified identifier.
data QualifiedIdentifier
  = GlobalIdentifier ModuleName Identifier
  | LocalIdentifier Identifier
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString QualifiedIdentifier where
  fromString = LocalIdentifier . fromString

-- | Note that these are partial functions.
instance IsList QualifiedIdentifier where
  type Item QualifiedIdentifier = Text

  fromList [] = error "more than 1 items necessary"
  fromList [t] = LocalIdentifier $ UserIdentifier t
  fromList ts =
    let
      n = ModuleName $ N.fromList $ init ts
      i = UserIdentifier $ last ts
    in GlobalIdentifier n i

  toList (GlobalIdentifier (ModuleName n) (UserIdentifier t)) = N.toList n ++ [t]
  toList (LocalIdentifier (UserIdentifier t))                 = [t]
  toList _                                                    = error "system identifiers are not acceptable"

instance Pretty QualifiedIdentifier where
  pretty (GlobalIdentifier n i) = pretty n <> "." <> pretty i
  pretty (LocalIdentifier i)    = pretty i

data EitherIdentifier
  = UnqualifiedIdentifier Identifier
  | QualifiedIdentifier QualifiedIdentifier
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString EitherIdentifier where
  fromString = UnqualifiedIdentifier . fromString

-- | Note that these are partial functions.
instance IsList EitherIdentifier where
  type Item EitherIdentifier = Text

  fromList []  = error "more than 1 items necessary"
  fromList [t] = UnqualifiedIdentifier $ UserIdentifier t
  fromList ts  = QualifiedIdentifier $ E.fromList ts

  toList (QualifiedIdentifier i)                    = E.toList i
  toList (UnqualifiedIdentifier (UserIdentifier t)) = [t]
  toList _                                          = error "system identifiers are not acceptable"

instance Pretty EitherIdentifier where
  pretty (UnqualifiedIdentifier i) = pretty i
  pretty (QualifiedIdentifier i)   = pretty i

type BindIdentifier :: NameResolving -> K.Type
type family BindIdentifier n where
  BindIdentifier 'NameUnresolved = Identifier

  BindIdentifier 'NameResolved = QualifiedIdentifier

type ReferenceIdentifier :: NameResolving -> K.Type
type family ReferenceIdentifier n where
  ReferenceIdentifier 'NameUnresolved = EitherIdentifier

  ReferenceIdentifier 'NameResolved = QualifiedIdentifier

-- ModuleName

-- | A module name.
newtype ModuleName = ModuleName (N.NonEmpty Text) deriving (Show, Read, Eq, Ord, Generic)

instance IsString ModuleName where
  fromString = ModuleName . (:| []) . fromString

instance IsList ModuleName where
  type Item ModuleName = Text
  fromList = ModuleName . N.fromList
  toList (ModuleName n) = N.toList n

instance Pretty ModuleName where
  pretty (ModuleName ts) = fold $ N.intersperse "." ts

-- Target

data Target
  = C
  deriving (Show, Read, Eq, Ord, Enum, Generic)

-- Locations

data Location =
  Location
    { filePath :: FilePath
    , begin    :: Position
    , end      :: Position
    }
  deriving (Show, Read, Eq, Ord, Generic)

-- | A position in a source file.
data Position =
  Position { line :: Word, column :: Word }
  deriving (Show, Read, Eq, Ord, Generic)

instance Pretty a => Pretty (With Location a) where
  pretty (With _ a) = pretty a

-- Kinds

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

data NameResolving
  = NameResolved
  | NameUnresolved
  deriving (Show, Read, Eq, Ord, Generic)

-- Higher kinded data

toIdentity :: (Functor f, Copointed f, FunctorB b) => f (b f) -> Identity (b Identity)
toIdentity = Identity . bmap (Identity . copoint) . copoint

-- Pretty

-- | A pretty-printable class.
class Pretty a where
  pretty :: a -> Text

instance Pretty a => Pretty [a] where
  pretty as = "(list " <> T.intercalate " " (pretty <$> as) <> ")"

instance Pretty a => Pretty (Identity a) where
  pretty (Identity a) = pretty a
