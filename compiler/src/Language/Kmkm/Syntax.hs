{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE EmptyDataDeriving        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
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
    -- * Metadata
  , HasPosition (..)
  , Position (..)
  , WithPosition (..)
    -- * Kinds and types
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  , NameResolving (..)
    -- * Pretty printing
  , Pretty (..)
  ) where

import qualified Language.Kmkm.Exception as X

import           Barbies               (Barbie)
import           Barbies               as B (Barbie (Barbie))
import           Barbies.Bare          (BareB (bcover, bstrip), Wear)
import qualified Barbies.Bare          as B
import           Data.Bifunctor        (Bifunctor (bimap))
import           Data.Copointed        (Copointed (copoint))
import           Data.Foldable         (Foldable (fold))
import           Data.Function         (on)
import           Data.Functor.Barbie   (FunctorB)
import           Data.Functor.Identity (Identity (Identity, runIdentity))
import qualified Data.Kind             as K
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as N
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Exts              (IsList)
import qualified GHC.Exts              as E
import           GHC.Generics          (Generic)
import qualified Language.C.Pretty     as C
import           Language.C.Syntax.AST (CExtDecl)
import qualified Text.PrettyPrint      as P

-- Module

type Module :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data Module n c l t b f =
    Module (Wear b f ModuleName) (Wear b f [Wear b f ModuleName]) (Wear b f [Wear b f (Definition n c l t b f)])
  deriving Generic

type ModuleConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type ModuleConstraint cls n c l t b f =
  ( cls (Wear b f ModuleName)
  , cls (Wear b f [Wear b f ModuleName])
  , cls (Wear b f (Definition n c l t b f))
  , cls (Wear b f [Wear b f (Definition n c l t b f)])
  )

deriving instance ModuleConstraint Show n c l t b f => Show (Module n c l t b f)
deriving instance ModuleConstraint Eq n c l t b f => Eq (Module n c l t b f)
deriving instance ModuleConstraint Ord n c l t b f => Ord (Module n c l t b f)

deriving via Barbie (Module n c l t B.Covered) instance FunctorB (Module n c l t B.Covered)

instance (BareB (ValueBind n c l t), BareB (FunctionType n c)) => BareB (Module n c l t) where
  bstrip (Module (Identity n) (Identity ms) (Identity ds)) = Module n (runIdentity <$> ms) (bstrip . runIdentity <$> ds)
  bcover (Module n ms ds) = Module (Identity n) (Identity $ Identity <$> ms) (Identity $ Identity . bcover <$> ds)

-- Definition

type Definition :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data Definition n c l t b f
  = DataDefinition (Wear b f (BindIdentifier n)) (Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f (Type n c b f))])])
  | TypeBind (Wear b f (BindIdentifier n)) (Wear b f (Type n c b f))
  | ValueBind (ValueBind n c l t b f)
  | ForeignTypeBind (Wear b f (BindIdentifier n)) (Wear b f [Wear b f CHeader]) (Wear b f CDefinition)
  | ForeignValueBind (Wear b f (BindIdentifier n)) (Wear b f [Wear b f CHeader]) (Wear b f CDefinition) (Wear b f (Type n c b f))
  deriving Generic

type DefinitionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type DefinitionConstraint cls n c l t b f =
  ( cls (Wear b f (BindIdentifier n))
  , cls (Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f (Type n c b f))])])
  , cls (Wear b f (Type n c b f))
  , cls (ValueBind n c l t b f)
  , cls (Wear b f CHeader)
  , cls (Wear b f [Wear b f CHeader])
  , cls (Wear b f CDefinition)
  )

deriving instance DefinitionConstraint Show n c l t b f => Show (Definition n c l t b f)
deriving instance DefinitionConstraint Eq n c l t b f => Eq (Definition n c l t b f)
deriving instance DefinitionConstraint Ord n c l t b f => Ord (Definition n c l t b f)

deriving via Barbie (Definition n c l t B.Covered) instance FunctorB (Definition n c l t B.Covered)

instance (BareB (ValueBind n c l t), BareB (FunctionType n c)) => BareB (Definition n c l t) where
  bstrip (DataDefinition (Identity i) (Identity cs)) =
    DataDefinition i (constructor <$> cs)
    where
      constructor (Identity (Identity i, Identity fs)) = (i, field <$> fs)
      field (Identity (Identity i, Identity t)) = (i, bstrip t)
  bstrip (TypeBind (Identity i) (Identity t)) = TypeBind i (bstrip t)
  bstrip (ValueBind b) = ValueBind (bstrip b)
  bstrip (ForeignTypeBind (Identity i) (Identity hs) (Identity c)) = ForeignTypeBind i (runIdentity <$> hs) c
  bstrip (ForeignValueBind (Identity i) (Identity hs) (Identity c) (Identity t)) = ForeignValueBind i (runIdentity <$> hs) c (bstrip t)

  bcover (DataDefinition i cs) =
    DataDefinition (Identity i) (Identity $ constructor <$> cs)
    where
      constructor (i, fs) = Identity (Identity i, Identity $ field <$> fs)
      field (i, t) = Identity (Identity i, Identity $ bcover t)
  bcover (TypeBind i t) = TypeBind (Identity i) (Identity $ bcover t)
  bcover (ValueBind b) = ValueBind (bcover b)
  bcover (ForeignTypeBind i hs c) = ForeignTypeBind (Identity i) (Identity $ Identity <$> hs) (Identity c)
  bcover (ForeignValueBind i hs c t) = ForeignValueBind (Identity i) (Identity $ Identity <$> hs) (Identity c) (Identity $ bcover t)

-- ValueBind

type ValueBind :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family ValueBind

deriving via Barbie (ValueBind n c l t B.Covered) instance FunctorB (ValueBind n c l t B.Covered)

type ValueBindConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family ValueBindConstraint cls n c l t b f where
  ValueBindConstraint cls n c 'LambdaUnlifted t b f =
    ( cls (Wear b f (BindIdentifier n))
    , cls (Wear b f (Value n c 'LambdaUnlifted t b f))
    )

  ValueBindConstraint cls n c 'LambdaLifted t b f =
    ( cls (Wear b f (BindIdentifier n))
    , cls (Wear b f (Value n c 'LambdaLifted t b f))
    , cls (Wear b f (Type n c b f))
    , cls (Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f (Type n c b f))])
    )

data instance ValueBind n c 'LambdaUnlifted t b f =
  ValueBindU (Wear b f (BindIdentifier n)) (Wear b f (Value n c 'LambdaUnlifted t b f))
  deriving Generic

deriving instance ValueBindConstraint Show n c 'LambdaUnlifted t b f => Show (ValueBind n c 'LambdaUnlifted t b f)
deriving instance ValueBindConstraint Eq n c 'LambdaUnlifted t b f => Eq (ValueBind n c 'LambdaUnlifted t b f)
deriving instance ValueBindConstraint Ord n c 'LambdaUnlifted t b f => Ord (ValueBind n c 'LambdaUnlifted t b f)

instance BareB (Value n c 'LambdaUnlifted t) => BareB (ValueBind n c 'LambdaUnlifted t) where
  bstrip (ValueBindU (Identity i) (Identity v)) = ValueBindU i (bstrip v)

  bcover (ValueBindU i v) = ValueBindU (Identity i) (Identity $ bcover v)

data instance ValueBind n c 'LambdaLifted t b f
  = ValueBindV (Wear b f (BindIdentifier n)) (Wear b f (Value n c 'LambdaLifted t b f))
  | ValueBindN (Wear b f (BindIdentifier n)) (Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f (Type n c b f))]) (Wear b f (Value n c 'LambdaLifted t b f))
  deriving Generic

deriving instance ValueBindConstraint Show n c 'LambdaLifted t b f => Show (ValueBind n c 'LambdaLifted t b f)
deriving instance ValueBindConstraint Eq n c 'LambdaLifted t b f => Eq (ValueBind n c 'LambdaLifted t b f)
deriving instance ValueBindConstraint Ord n c 'LambdaLifted t b f => Ord (ValueBind n c 'LambdaLifted t b f)

instance (BareB (Value n c 'LambdaLifted t), BareB (FunctionType n c)) => BareB (ValueBind n c 'LambdaLifted t) where
  bstrip (ValueBindV (Identity i) (Identity v)) = ValueBindV i (bstrip v)
  bstrip (ValueBindN (Identity i) (Identity ps) (Identity v)) =
    ValueBindN i (parameter <$> ps) (bstrip v)
    where
      parameter (Identity (Identity i, Identity t)) = (i, bstrip t)

  bcover (ValueBindV i v) = ValueBindV (Identity i) (Identity $ bcover v)
  bcover (ValueBindN i ps v) =
    ValueBindN (Identity i) (Identity $ parameter <$> ps) (Identity $ bcover v)
    where
      parameter (i, t) = Identity (Identity i, Identity $ bcover t)

-- CDefinition

newtype CDefinition = CDefinition CExtDecl deriving (Show, Generic)

instance Eq CDefinition where
  (==) = (==) `on` (\(CDefinition c) -> P.render $ C.pretty c)

instance Ord CDefinition where
  compare = compare `on` (\(CDefinition c) -> P.render $ C.pretty c)

-- CHeader

data CHeader
  = SystemHeader Text
  | LocalHeader Text
  deriving (Show, Read, Eq, Ord, Generic)

-- Type

type Type :: NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Type
data Type n c b f
  = TypeVariable (Wear b f (ReferenceIdentifier n))
  | TypeApplication (Wear b f (Type n c b f)) (Wear b f (Type n c b f))
  | FunctionType (FunctionType n c b f)
  | ProcedureType (Wear b f (Type n c b f))
  deriving Generic

type TypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type TypeConstraint cls n c b f =
  ( cls (Wear b f (ReferenceIdentifier n))
  , cls (FunctionType n c b f)
  , cls (Wear b f (Type n c b f))
  )

deriving instance TypeConstraint Show n c b f => Show (Type n c b f)
deriving instance TypeConstraint Read n c b f => Read (Type n c b f)
deriving instance TypeConstraint Eq n c b f => Eq (Type n c b f)
deriving instance TypeConstraint Ord n c b f => Ord (Type n c b f)

deriving via Barbie (Type n c B.Covered) instance FunctorB (Type n c B.Covered)

instance BareB (FunctionType n c) => BareB (Type n c) where
  bstrip (TypeVariable (Identity i))                   = TypeVariable i
  bstrip (TypeApplication (Identity t1) (Identity t2)) = TypeApplication (bstrip t1) (bstrip t2)
  bstrip (FunctionType f)                              = FunctionType (bstrip f)
  bstrip (ProcedureType (Identity t))                  = ProcedureType (bstrip t)

  bcover (TypeVariable i)        = TypeVariable $ Identity i
  bcover (TypeApplication t1 t2) = TypeApplication (Identity $ bcover t1) (Identity $ bcover t2)
  bcover (FunctionType f)        = FunctionType $ bcover f
  bcover (ProcedureType t)       = ProcedureType $ Identity $ bcover t

-- FunctionType

type FunctionType :: NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Type
data family FunctionType

deriving via Barbie (FunctionType n c B.Covered) instance FunctorB (FunctionType n c B.Covered)

type FunctionTypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family FunctionTypeConstraint cls n c b f where
  FunctionTypeConstraint cls n 'Curried b f =
    ( cls (Wear b f (ReferenceIdentifier n))
    , cls (Wear b f (Type n 'Curried b f))
    )

  FunctionTypeConstraint cls n 'Uncurried b f =
    ( cls (Wear b f (ReferenceIdentifier n))
    , cls (Wear b f (Type n 'Uncurried b f))
    , cls (Wear b f [Wear b f (Type n 'Uncurried b f)])
    )

data instance FunctionType n 'Curried b f =
  FunctionTypeC (Wear b f (Type n 'Curried b f)) (Wear b f (Type n 'Curried b f))
  deriving Generic

deriving instance FunctionTypeConstraint Show n 'Curried b f => Show (FunctionType n 'Curried b f)
deriving instance FunctionTypeConstraint Read n 'Curried b f => Read (FunctionType n 'Curried b f)
deriving instance FunctionTypeConstraint Eq n 'Curried b f => Eq (FunctionType n 'Curried b f)
deriving instance FunctionTypeConstraint Ord n 'Curried b f => Ord (FunctionType n 'Curried b f)

instance BareB (FunctionType n 'Curried) where
  bstrip (FunctionTypeC (Identity t1) (Identity t2)) = FunctionTypeC (bstrip t1) (bstrip t2)
  bcover (FunctionTypeC t1 t2) = FunctionTypeC (Identity $ bcover t1) (Identity $ bcover t2)

data instance FunctionType n 'Uncurried b f =
  FunctionTypeN (Wear b f [Wear b f (Type n 'Uncurried b f)]) (Wear b f (Type n 'Uncurried b f))
  deriving Generic

deriving instance FunctionTypeConstraint Show n 'Uncurried b f => Show (FunctionType n 'Uncurried b f)
deriving instance FunctionTypeConstraint Read n 'Uncurried b f => Read (FunctionType n 'Uncurried b f)
deriving instance FunctionTypeConstraint Eq n 'Uncurried b f => Eq (FunctionType n 'Uncurried b f)
deriving instance FunctionTypeConstraint Ord n 'Uncurried b f => Ord (FunctionType n 'Uncurried b f)

instance BareB (FunctionType n 'Uncurried) where
  bstrip (FunctionTypeN (Identity ts) (Identity t)) = FunctionTypeN (bstrip . runIdentity <$> ts) (bstrip t)
  bcover (FunctionTypeN ts t) = FunctionTypeN (Identity $ Identity . bcover <$> ts) (Identity $ bcover t)

-- Value

type Value :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family Value

deriving via Barbie (Value n c l t B.Covered) instance FunctorB (Value n c l t B.Covered)

type ValueConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family ValueConstraint cls n c l t b f where
  ValueConstraint cls n c l 'Untyped b f =
    ( cls (Wear b f (Value' n c l 'Untyped b f))
    , cls (Wear b f (ReferenceIdentifier n))
    , cls (Wear b f (BindIdentifier n))
    , cls (Wear b f (Literal n c l 'Untyped b f))
    )

  ValueConstraint cls n c l 'Typed b f =
    ( cls (Wear b f (Value' n c l 'Typed b f))
    , cls (Wear b f (ReferenceIdentifier n))
    , cls (Wear b f (BindIdentifier n))
    , cls (Wear b f (Literal n c l 'Typed b f))
    , cls (Wear b f (Type n c b f))
    )

newtype instance Value n c l 'Untyped b f =
  UntypedValue (Wear b f (Value' n c l 'Untyped b f))
  deriving Generic

deriving instance ValueConstraint Show n c l 'Untyped b f => Show (Value n c l 'Untyped b f)
deriving instance ValueConstraint Eq n c l 'Untyped b f => Eq (Value n c l 'Untyped b f)
deriving instance ValueConstraint Ord n c l 'Untyped b f => Ord (Value n c l 'Untyped b f)

instance
  ( BareB (Application n c l 'Untyped)
  , BareB (ProcedureStep n c l 'Untyped)
  , BareB (TypeAnnotation n c l 'Untyped)
  , BareB (Definition n c l 'Untyped)
  , BareB (Function n c l 'Untyped)
  ) =>
  BareB (Value n c l 'Untyped) where

  bstrip (UntypedValue (Identity v)) = UntypedValue $ bstrip v
  bcover (UntypedValue v) = UntypedValue $ Identity $ bcover v

data instance Value n c l 'Typed b f =
  TypedValue (Wear b f (Value' n c l 'Typed b f)) (Wear b f (Type n c b f))
  deriving Generic

deriving instance ValueConstraint Show n c l 'Typed b f => Show (Value n c l 'Typed b f)
deriving instance ValueConstraint Eq n c l 'Typed b f => Eq (Value n c l 'Typed b f)
deriving instance ValueConstraint Ord n c l 'Typed b f => Ord (Value n c l 'Typed b f)

instance
  ( BareB (FunctionType n c)
  , BareB (Application n c l 'Typed)
  , BareB (ProcedureStep n c l 'Typed)
  , BareB (TypeAnnotation n c l 'Typed)
  , BareB (Definition n c l 'Typed)
  , BareB (Function n c l 'Typed)
  ) =>
  BareB (Value n c l 'Typed) where

  bstrip (TypedValue (Identity v) (Identity t)) = TypedValue (bstrip v) (bstrip t)
  bcover (TypedValue v t) = TypedValue (Identity $ bcover v) (Identity $ bcover t)

-- Value'

data Value' n c l t b f
  = Variable (Wear b f (ReferenceIdentifier n))
  | Literal (Literal n c l t b f)
  | Application (Application n c l t b f)
  | Procedure (Wear b f (NonEmpty (Wear b f (ProcedureStep n c l t b f))))
  | TypeAnnotation (TypeAnnotation n c l t b f)
  | Let (Wear b f [Wear b f (Definition n c l t b f)]) (Wear b f (Value n c l t b f))
  deriving Generic

type Value'Constraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type Value'Constraint cls n c l t b f =
  ( cls (Wear b f (ReferenceIdentifier n))
  , cls (Wear b f (BindIdentifier n))
  , cls (Literal n c l t b f)
  , cls (Application n c l t b f)
  , cls (Wear b f (ProcedureStep n c l t b f))
  , cls (Wear b f (NonEmpty (Wear b f (ProcedureStep n c l t b f))))
  , cls (TypeAnnotation n c l t b f)
  , cls (Wear b f (ValueBind n c l t b f))
  , cls (Wear b f (Value n c l t b f))
  , cls (Wear b f (Type n c b f))
  , cls (Wear b f (Definition n c l t b f))
  , cls (Wear b f [Wear b f (Definition n c l t b f)])
  , cls (Wear b f (ProcedureStep n c l t b f))
  )

deriving instance Value'Constraint Show n c l t b f => Show (Value' n c l t b f)
deriving instance Value'Constraint Eq n c l t b f => Eq (Value' n c l t b f)
deriving instance Value'Constraint Ord n c l t b f => Ord (Value' n c l t b f)

deriving via Barbie (Value' n c l t B.Covered) instance FunctorB (Value' n c l t B.Covered)

instance (BareB (Application n c l t), BareB (ProcedureStep n c l t), BareB (TypeAnnotation n c l t), BareB (Value n c l t), BareB (Definition n c l t), BareB (Function n c l t)) => BareB (Value' n c l t) where
  bstrip (Variable (Identity i))          = Variable i
  bstrip (Literal l)                      = Literal $ bstrip l
  bstrip (Application a)                  = Application $ bstrip a
  bstrip (Procedure (Identity ss))        = Procedure $ bstrip . runIdentity <$> ss
  bstrip (TypeAnnotation a)               = TypeAnnotation $ bstrip a
  bstrip (Let (Identity ds) (Identity v)) = Let (bstrip . runIdentity <$> ds) (bstrip v)

  bcover (Variable i)       = Variable $ Identity i
  bcover (Literal l)        = Literal $ bcover l
  bcover (Application a)    = Application $ bcover a
  bcover (Procedure ss)     = Procedure $ Identity $ Identity . bcover <$> ss
  bcover (TypeAnnotation a) = TypeAnnotation $ bcover a
  bcover (Let ds v)         = Let (Identity $ Identity . bcover <$> ds) (Identity $ bcover v)

-- TypeAnnotation

type TypeAnnotation :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family TypeAnnotation

deriving via Barbie (TypeAnnotation n c l t B.Covered) instance FunctorB (TypeAnnotation n c l t B.Covered)

data instance TypeAnnotation n c l 'Untyped b f =
  TypeAnnotation' (Wear b f (Value n c l 'Untyped b f)) (Wear b f (Type n c b f))
  deriving Generic

type TypeAnnotationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type TypeAnnotationConstraint cls n c l t b f =
  ( cls (Wear b f (ReferenceIdentifier n))
  , cls (Wear b f (BindIdentifier n))
  , cls (Wear b f (Function n c l t b f))
  , cls (Wear b f (Application n c l t b f))
  , cls (Wear b f (ProcedureStep n c l t b f))
  , cls (Wear b f (ValueBind n c l t b f))
  , cls (Wear b f (FunctionType n c b f))
  , cls (Wear b f (Value n c l t b f))
  , cls (Wear b f (Type n c b f))
  )

deriving instance TypeAnnotationConstraint Show n c l 'Untyped b f => Show (TypeAnnotation n c l 'Untyped b f)
deriving instance TypeAnnotationConstraint Eq n c l 'Untyped b f => Eq (TypeAnnotation n c l 'Untyped b f)
deriving instance TypeAnnotationConstraint Ord n c l 'Untyped b f => Ord (TypeAnnotation n c l 'Untyped b f)

instance
  ( BareB (Application n c l 'Untyped)
  , BareB (ValueBind n c l 'Untyped)
  , BareB (Function n c l 'Untyped)
  , BareB (FunctionType n c)
  ) =>
  BareB (TypeAnnotation n c l 'Untyped) where

  bstrip (TypeAnnotation' (Identity v) (Identity t)) = TypeAnnotation' (bstrip v) (bstrip t)

  bcover (TypeAnnotation' v t) = TypeAnnotation' (Identity $ bcover v) (Identity $ bcover t)

data instance TypeAnnotation n c l 'Typed b f
  deriving Generic

deriving instance Show (TypeAnnotation n c l 'Typed b f)
deriving instance TypeAnnotationConstraint Eq n c l 'Typed b f => Eq (TypeAnnotation n c l 'Typed b f)
deriving instance TypeAnnotationConstraint Ord n c l 'Typed b f => Ord (TypeAnnotation n c l 'Typed b f)

instance BareB (TypeAnnotation n c l 'Typed) where
  bstrip = X.unreachable
  bcover = X.unreachable

-- ProcedureStep

data ProcedureStep n c l t b f
  = BindProcedure (Wear b f (BindIdentifier n)) (Wear b f (Value n c l t b f))
  | TermProcedure (Wear b f (Value n c l t b f))

type ProcedureStepConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type ProcedureStepConstraint cls n c l t b f =
  ( cls (Wear b f (BindIdentifier n))
  , cls (Wear b f (Value n c l t b f))
  )

deriving instance ProcedureStepConstraint Show n c l t b f => Show (ProcedureStep n c l t b f)
deriving instance ProcedureStepConstraint Eq n c l t b f => Eq (ProcedureStep n c l t b f)
deriving instance ProcedureStepConstraint Ord n c l t b f => Ord (ProcedureStep n c l t b f)

deriving via Barbie (ProcedureStep n c l t B.Covered) instance FunctorB (ProcedureStep n c l t B.Covered)

instance (BareB (Value n c l t)) => BareB (ProcedureStep n c l t) where
  bstrip (BindProcedure (Identity i) (Identity v)) = BindProcedure i (bstrip v)
  bstrip (TermProcedure (Identity v))              = TermProcedure (bstrip v)

  bcover (BindProcedure i v) = BindProcedure (Identity i) (Identity $ bcover v)
  bcover (TermProcedure v)   = TermProcedure (Identity $ bcover v)

-- Literal

data Literal n c l t b f
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function (Function n c l t b f)
  deriving Generic

type LiteralConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type LiteralConstraint cls n c l t b f = cls (Function n c l t b f)

deriving instance LiteralConstraint Show n c l t b f => Show (Literal n c l t b f)
deriving instance LiteralConstraint Eq n c l t b f => Eq (Literal n c l t b f)
deriving instance LiteralConstraint Ord n c l t b f => Ord (Literal n c l t b f)

deriving via Barbie (Literal n c l t B.Covered) instance FunctorB (Literal n c l t B.Covered)

instance BareB (Function n c l t) => BareB (Literal n c l t) where
  bstrip (Integer v b)      = Integer v b
  bstrip (Fraction s f e b) = Fraction s f e b
  bstrip (String s)         = String s
  bstrip (Function f)       = Function $ bstrip f

  bcover (Integer v b)      = Integer v b
  bcover (Fraction s f e b) = Fraction s f e b
  bcover (String s)         = String s
  bcover (Function f)       = Function $ bcover f

-- Application

type Application :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family Application

deriving via Barbie (Application n c l t B.Covered) instance FunctorB (Application n c l t B.Covered)

type ApplicationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family ApplicationConstraint cls n c l t b f where
  ApplicationConstraint cls n 'Curried l t b f =
    (cls (Wear b f (Value n 'Curried l t b f)))

  ApplicationConstraint cls n 'Uncurried l t b f =
    ( cls (Wear b f (Value n 'Uncurried l t b f))
    , cls (Wear b f [Wear b f (Value n 'Uncurried l t b f)])
    )

data instance Application n 'Curried l t b f =
  ApplicationC (Wear b f (Value n 'Curried l t b f)) (Wear b f (Value n 'Curried l t b f))
  deriving Generic

deriving instance ApplicationConstraint Show n 'Curried l t b f => Show (Application n 'Curried l t b f)
deriving instance ApplicationConstraint Eq n 'Curried l t b f => Eq (Application n 'Curried l t b f)
deriving instance ApplicationConstraint Ord n 'Curried l t b f => Ord (Application n 'Curried l t b f)

instance BareB (Value n 'Curried l t) => BareB (Application n 'Curried l t) where
  bstrip (ApplicationC (Identity v1) (Identity v2)) = ApplicationC (bstrip v1) (bstrip v2)
  bcover (ApplicationC v1 v2) = ApplicationC (Identity $ bcover v1) (Identity $ bcover v2)

data instance Application n 'Uncurried l t b f =
  ApplicationN (Wear b f (Value n 'Uncurried l t b f)) (Wear b f [Wear b f (Value n 'Uncurried l t b f)])
  deriving Generic

deriving instance ApplicationConstraint Show n 'Uncurried l t b f => Show (Application n 'Uncurried l t b f)
deriving instance ApplicationConstraint Eq n 'Uncurried l t b f => Eq (Application n 'Uncurried l t b f)
deriving instance ApplicationConstraint Ord n 'Uncurried l t b f => Ord (Application n 'Uncurried l t b f)

instance BareB (Value n 'Uncurried l t) => BareB (Application n 'Uncurried l t) where
  bstrip (ApplicationN (Identity v) (Identity vs)) = ApplicationN (bstrip v) (bstrip . runIdentity <$> vs)
  bcover (ApplicationN v vs) = ApplicationN (Identity $ bcover v) (Identity $ Identity . bcover <$> vs)

-- Function

type Function :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family Function

deriving via Barbie (Function n c l t B.Covered) instance FunctorB (Function n c l t B.Covered)

type FunctionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family FunctionConstraint cls n c l t b f where
  FunctionConstraint cls n 'Curried l t b f =
    ( cls (Wear b f (BindIdentifier n))
    , cls (Wear b f (ReferenceIdentifier n))
    , cls (Wear b f (Value n 'Curried l t b f))
    , cls (Wear b f (Type n 'Curried b f))
    )

  FunctionConstraint cls n 'Uncurried l t b f =
    ( cls (Wear b f (BindIdentifier n))
    , cls (Wear b f (ReferenceIdentifier n))
    , cls (Wear b f (Value n 'Uncurried l t b f))
    , cls (Wear b f (Type n 'Uncurried b f))
    , cls (Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f (Type n 'Uncurried b f))])
    )

data instance Function n 'Curried 'LambdaUnlifted t b f =
  FunctionC (Wear b f (BindIdentifier n)) (Wear b f (Type n 'Curried b f)) (Wear b f (Value n 'Curried 'LambdaUnlifted t b f))
  deriving Generic

deriving instance FunctionConstraint Show n 'Curried 'LambdaUnlifted t b f => Show (Function n 'Curried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Eq n 'Curried 'LambdaUnlifted t b f => Eq (Function n 'Curried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Ord n 'Curried 'LambdaUnlifted t b f => Ord (Function n 'Curried 'LambdaUnlifted t b f)

instance BareB (Value n 'Curried 'LambdaUnlifted t) => BareB (Function n 'Curried 'LambdaUnlifted t) where
  bstrip (FunctionC (Identity i) (Identity t) (Identity v)) = FunctionC i (bstrip t) (bstrip v)
  bcover (FunctionC i t v) = FunctionC (Identity i) (Identity $ bcover t) (Identity $ bcover v)

data instance Function n 'Uncurried 'LambdaUnlifted t b f =
  FunctionN (Wear b f [Wear b f (Wear b f (BindIdentifier n), Wear b f (Type n 'Uncurried b f))]) (Wear b f (Value n 'Uncurried 'LambdaUnlifted t b f))
  deriving Generic

deriving instance FunctionConstraint Show n 'Uncurried 'LambdaUnlifted t b f => Show (Function n 'Uncurried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Eq n 'Uncurried 'LambdaUnlifted t b f => Eq (Function n 'Uncurried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Ord n 'Uncurried 'LambdaUnlifted t b f => Ord (Function n 'Uncurried 'LambdaUnlifted t b f)

instance BareB (Value n 'Uncurried 'LambdaUnlifted t) => BareB (Function n 'Uncurried 'LambdaUnlifted t) where
  bstrip (FunctionN (Identity ps) (Identity v)) = FunctionN (bimap runIdentity (bstrip . runIdentity) . runIdentity <$> ps) (bstrip v)
  bcover (FunctionN ps v) = FunctionN (Identity $ Identity . bimap Identity (Identity . bcover) <$> ps) (Identity $ bcover v)

data instance Function n c 'LambdaLifted t b f deriving (Show, Read, Eq, Ord, Generic)

instance BareB (Function n c 'LambdaLifted t) where
  bstrip = X.unreachable
  bcover = X.unreachable

-- Identifiers

data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = UserIdentifier . fromString

instance Pretty Identifier where
  pretty (UserIdentifier t)     = t
  pretty (SystemIdentifier c n) = "_" <> T.pack [c] <> T.pack (show n)

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

newtype ModuleName = ModuleName (N.NonEmpty Text) deriving (Show, Read, Eq, Ord, Generic)

instance IsString ModuleName where
  fromString = ModuleName . (:| []) . fromString

instance IsList ModuleName where
  type Item ModuleName = Text
  fromList = ModuleName . N.fromList
  toList (ModuleName n) = N.toList n

instance Pretty ModuleName where
  pretty (ModuleName ts) = fold $ N.intersperse "." ts

-- Positions

class HasPosition f where
  range :: f a -> Maybe (Position, Position)

data Position =
  Position { line :: Word, column :: Word }
  deriving (Show, Read, Eq, Ord, Generic)

data WithPosition a =
  WithPosition Position Position a
  deriving (Show, Read, Eq, Ord, Generic)

instance Functor WithPosition where
  fmap f (WithPosition p1 p2 a) = WithPosition p1 p2 $ f a

instance Foldable WithPosition where
  foldMap f (WithPosition _ _ a) = f a

instance Traversable WithPosition where
  traverse f (WithPosition p1 p2 a) = WithPosition p1 p2 <$> f a

instance Copointed WithPosition where
  copoint (WithPosition _ _ a) = a

instance HasPosition WithPosition where
  range (WithPosition begin end _) = Just (begin, end)

instance HasPosition Identity where
  range (Identity _) = Nothing

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

-- Pretty

class Pretty a where
  pretty :: a -> Text
