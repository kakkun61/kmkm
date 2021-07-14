{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
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

#if __GLASGOW_HASKELL__ < 902
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
    -- * Higher kinded data
  , strip
    -- * Pretty printing
  , Pretty (..)
  ) where

import qualified Language.Kmkm.Exception as X

import           Barbies.Bare.Layered        (BareB (bcover, bstrip), bstripFrom)
import qualified Barbies.Bare.Layered        as B
import           Data.Bifunctor              (Bifunctor (bimap))
import           Data.Copointed              (Copointed (copoint))
import           Data.Foldable               (Foldable (fold))
import           Data.Function               (on)
import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Identity       (Identity (Identity, runIdentity))
import qualified Data.Kind                   as K
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import qualified Data.List.NonEmpty          as N
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Exts                    (IsList)
import qualified GHC.Exts                    as E
import           GHC.Generics                (Generic)
import qualified Language.C.Pretty           as C
import           Language.C.Syntax.AST       (CExtDecl)
import qualified Text.PrettyPrint            as P

-- Module

type Module :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data Module n c l t b f =
    Module (B.Wear b f ModuleName) (B.Wear b f [B.Wear b f ModuleName]) (B.Wear b f [B.Wear b f (Definition n c l t b f)])
  deriving Generic

type ModuleConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type ModuleConstraint cls n c l t b f =
  ( cls (B.Wear b f ModuleName)
  , cls (B.Wear b f [B.Wear b f ModuleName])
  , cls (B.Wear b f (Definition n c l t b f))
  , cls (B.Wear b f [B.Wear b f (Definition n c l t b f)])
  )

deriving instance ModuleConstraint Show n c l t b f => Show (Module n c l t b f)
deriving instance ModuleConstraint Eq n c l t b f => Eq (Module n c l t b f)
deriving instance ModuleConstraint Ord n c l t b f => Ord (Module n c l t b f)

instance (FunctorB (FunctionType n c B.Covered), FunctorB (ValueBind n c l t B.Covered)) => FunctorB (Module n c l t B.Covered) where
  bmap f (Module n ms ds) = Module (f n) (fmap f <$> f ms) (fmap (fmap (bmap f) . f) <$> f ds)

instance (BareB (ValueBind n c l t), BareB (FunctionType n c)) => BareB (Module n c l t) where
  bstrip (Module (Identity n) (Identity ms) (Identity ds)) = Module n (runIdentity <$> ms) (bstrip . runIdentity <$> ds)
  bcover (Module n ms ds) = Module (Identity n) (Identity $ Identity <$> ms) (Identity $ Identity . bcover <$> ds)

-- Definition

type Definition :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data Definition n c l t b f
  = DataDefinition (B.Wear b f (BindIdentifier n)) (B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f (Type n c b f))])])
  | TypeBind (B.Wear b f (BindIdentifier n)) (B.Wear b f (Type n c b f))
  | ValueBind (ValueBind n c l t b f)
  | ForeignTypeBind (B.Wear b f (BindIdentifier n)) (B.Wear b f [B.Wear b f CHeader]) (B.Wear b f CDefinition)
  | ForeignValueBind (B.Wear b f (BindIdentifier n)) (B.Wear b f [B.Wear b f CHeader]) (B.Wear b f CDefinition) (B.Wear b f (Type n c b f))
  deriving Generic

type DefinitionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type DefinitionConstraint cls n c l t b f =
  ( cls (B.Wear b f (BindIdentifier n))
  , cls (B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f (Type n c b f))])])
  , cls (B.Wear b f (Type n c b f))
  , cls (ValueBind n c l t b f)
  , cls (B.Wear b f CHeader)
  , cls (B.Wear b f [B.Wear b f CHeader])
  , cls (B.Wear b f CDefinition)
  )

deriving instance DefinitionConstraint Show n c l t b f => Show (Definition n c l t b f)
deriving instance DefinitionConstraint Eq n c l t b f => Eq (Definition n c l t b f)
deriving instance DefinitionConstraint Ord n c l t b f => Ord (Definition n c l t b f)

instance (FunctorB (Type n c B.Covered), FunctorB (ValueBind n c l t B.Covered)) => FunctorB (Definition n c l t B.Covered) where
  bmap :: forall f g. (Functor f, Functor g) => (forall a. f a -> g a) -> Definition n c l t B.Covered f -> Definition n c l t B.Covered g
  bmap f (DataDefinition n cs) =
    DataDefinition (f n) $ fmap constructor <$> f cs
    where
      constructor :: f (f (BindIdentifier n), f [f (f (BindIdentifier n), f (Type n c B.Covered f))]) -> g (g (BindIdentifier n), g [g (g (BindIdentifier n), g (Type n c B.Covered g))])
      constructor = fmap (bimap f $ fmap (fmap field) . f) . f
      field :: f (f (BindIdentifier n), f (Type n c B.Covered f)) -> g (g (BindIdentifier n), g (Type n c B.Covered g))
      field = fmap (bimap f (fmap (bmap f) . f)) . f
  bmap f (TypeBind i t) = TypeBind (f i) (bmap f <$> f t)
  bmap f (ValueBind b) = ValueBind (bmap f b)
  bmap f (ForeignTypeBind i hs c) =  ForeignTypeBind (f i) (fmap f <$> f hs) (f c)
  bmap f (ForeignValueBind i hs c t) = ForeignValueBind (f i) (fmap f <$> f hs) (f c) (bmap f <$> f t)

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

type ValueBindConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family ValueBindConstraint cls n c l t b f where
  ValueBindConstraint cls n c 'LambdaUnlifted t b f =
    ( cls (B.Wear b f (BindIdentifier n))
    , cls (B.Wear b f (Value n c 'LambdaUnlifted t b f))
    )

  ValueBindConstraint cls n c 'LambdaLifted t b f =
    ( cls (B.Wear b f (BindIdentifier n))
    , cls (B.Wear b f (Value n c 'LambdaLifted t b f))
    , cls (B.Wear b f (Type n c b f))
    , cls (B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f (Type n c b f))])
    )

data instance ValueBind n c 'LambdaUnlifted t b f =
  ValueBindU (B.Wear b f (BindIdentifier n)) (B.Wear b f (Value n c 'LambdaUnlifted t b f))
  deriving Generic

deriving instance ValueBindConstraint Show n c 'LambdaUnlifted t b f => Show (ValueBind n c 'LambdaUnlifted t b f)
deriving instance ValueBindConstraint Eq n c 'LambdaUnlifted t b f => Eq (ValueBind n c 'LambdaUnlifted t b f)
deriving instance ValueBindConstraint Ord n c 'LambdaUnlifted t b f => Ord (ValueBind n c 'LambdaUnlifted t b f)

instance FunctorB (Value n c 'LambdaUnlifted t B.Covered) => FunctorB (ValueBind n c 'LambdaUnlifted t B.Covered) where
  bmap f (ValueBindU i v) = ValueBindU (f i) (bmap f <$> f v)

instance BareB (Value n c 'LambdaUnlifted t) => BareB (ValueBind n c 'LambdaUnlifted t) where
  bstrip (ValueBindU (Identity i) (Identity v)) = ValueBindU i (bstrip v)

  bcover (ValueBindU i v) = ValueBindU (Identity i) (Identity $ bcover v)

data instance ValueBind n c 'LambdaLifted t b f
  = ValueBindV (B.Wear b f (BindIdentifier n)) (B.Wear b f (Value n c 'LambdaLifted t b f))
  | ValueBindN (B.Wear b f (BindIdentifier n)) (B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f (Type n c b f))]) (B.Wear b f (Value n c 'LambdaLifted t b f))
  deriving Generic

deriving instance ValueBindConstraint Show n c 'LambdaLifted t b f => Show (ValueBind n c 'LambdaLifted t b f)
deriving instance ValueBindConstraint Eq n c 'LambdaLifted t b f => Eq (ValueBind n c 'LambdaLifted t b f)
deriving instance ValueBindConstraint Ord n c 'LambdaLifted t b f => Ord (ValueBind n c 'LambdaLifted t b f)

instance (FunctorB (Value n c 'LambdaLifted t B.Covered), FunctorB (FunctionType n c B.Covered)) => FunctorB (ValueBind n c 'LambdaLifted t B.Covered) where
  bmap f (ValueBindV i v) = ValueBindV (f i) (bmap f <$> f v)
  bmap f (ValueBindN i ps v) = ValueBindN (f i) (fmap (fmap (bimap f (fmap (bmap f) . f)) . f) <$> f ps) (bmap f <$> f v)

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
  = TypeVariable (B.Wear b f (ReferenceIdentifier n))
  | TypeApplication (B.Wear b f (Type n c b f)) (B.Wear b f (Type n c b f))
  | FunctionType (FunctionType n c b f)
  | ProcedureType (B.Wear b f (Type n c b f))
  deriving Generic

type TypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type TypeConstraint cls n c b f =
  ( cls (B.Wear b f (ReferenceIdentifier n))
  , cls (FunctionType n c b f)
  , cls (B.Wear b f (Type n c b f))
  )

deriving instance TypeConstraint Show n c b f => Show (Type n c b f)
deriving instance TypeConstraint Read n c b f => Read (Type n c b f)
deriving instance TypeConstraint Eq n c b f => Eq (Type n c b f)
deriving instance TypeConstraint Ord n c b f => Ord (Type n c b f)

instance FunctorB (FunctionType n c B.Covered) => FunctorB (Type n c B.Covered) where
  bmap f (TypeVariable i)        = TypeVariable (f i)
  bmap f (TypeApplication t1 t2) = TypeApplication (bmap f <$> f t1) (bmap f <$> f t2)
  bmap f (FunctionType g)        = FunctionType (bmap f g)
  bmap f (ProcedureType t)       = ProcedureType (bmap f <$> f t)

instance BareB (FunctionType n c) => BareB (Type n c) where
  bstrip (TypeVariable (Identity i))                   = TypeVariable i
  bstrip (TypeApplication (Identity t1) (Identity t2)) = TypeApplication (bstrip t1) (bstrip t2)
  bstrip (FunctionType f)                              = FunctionType (bstrip f)
  bstrip (ProcedureType (Identity t))                  = ProcedureType (bstrip t)

  bcover (TypeVariable i)        = TypeVariable $ Identity i
  bcover (TypeApplication t1 t2) = TypeApplication (Identity $ bcover t1) (Identity $ bcover t2)
  bcover (FunctionType f)        = FunctionType $ bcover f
  bcover (ProcedureType t)       = ProcedureType $ Identity $ bcover t

instance
  ( Pretty (B.Wear b f (Type n c b f))
  , Pretty (B.Wear b f (ReferenceIdentifier n))
  , Pretty (FunctionType n c b f)
  ) =>
  Pretty (Type n c b f) where

  pretty (TypeVariable i)        = pretty i
  pretty (TypeApplication t1 t2) = "(apply " <> pretty t1 <> " " <> pretty t2 <> ")"
  pretty (FunctionType f)        = pretty f
  pretty (ProcedureType t)       = "(procedure " <> pretty t <> ")"

-- FunctionType

type FunctionType :: NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Type
data family FunctionType

type FunctionTypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family FunctionTypeConstraint cls n c b f where
  FunctionTypeConstraint cls n 'Curried b f =
    ( cls (B.Wear b f (ReferenceIdentifier n))
    , cls (B.Wear b f (Type n 'Curried b f))
    )

  FunctionTypeConstraint cls n 'Uncurried b f =
    ( cls (B.Wear b f (ReferenceIdentifier n))
    , cls (B.Wear b f (Type n 'Uncurried b f))
    , cls (B.Wear b f [B.Wear b f (Type n 'Uncurried b f)])
    )

data instance FunctionType n 'Curried b f =
  FunctionTypeC (B.Wear b f (Type n 'Curried b f)) (B.Wear b f (Type n 'Curried b f))
  deriving Generic

deriving instance FunctionTypeConstraint Show n 'Curried b f => Show (FunctionType n 'Curried b f)
deriving instance FunctionTypeConstraint Read n 'Curried b f => Read (FunctionType n 'Curried b f)
deriving instance FunctionTypeConstraint Eq n 'Curried b f => Eq (FunctionType n 'Curried b f)
deriving instance FunctionTypeConstraint Ord n 'Curried b f => Ord (FunctionType n 'Curried b f)

instance FunctorB (FunctionType n 'Curried B.Covered) where
  bmap f (FunctionTypeC t1 t2) = FunctionTypeC (bmap f <$> f t1) (bmap f <$> f t2)

instance BareB (FunctionType n 'Curried) where
  bstrip (FunctionTypeC (Identity t1) (Identity t2)) = FunctionTypeC (bstrip t1) (bstrip t2)
  bcover (FunctionTypeC t1 t2) = FunctionTypeC (Identity $ bcover t1) (Identity $ bcover t2)

instance Pretty (B.Wear b f (Type n 'Curried b f)) => Pretty (FunctionType n 'Curried b f) where
  pretty (FunctionTypeC t1 t2) = "(function " <> pretty t1 <> " " <> pretty t2 <> ")"

data instance FunctionType n 'Uncurried b f =
  FunctionTypeN (B.Wear b f [B.Wear b f (Type n 'Uncurried b f)]) (B.Wear b f (Type n 'Uncurried b f))
  deriving Generic

deriving instance FunctionTypeConstraint Show n 'Uncurried b f => Show (FunctionType n 'Uncurried b f)
deriving instance FunctionTypeConstraint Read n 'Uncurried b f => Read (FunctionType n 'Uncurried b f)
deriving instance FunctionTypeConstraint Eq n 'Uncurried b f => Eq (FunctionType n 'Uncurried b f)
deriving instance FunctionTypeConstraint Ord n 'Uncurried b f => Ord (FunctionType n 'Uncurried b f)

instance FunctorB (FunctionType n 'Uncurried B.Covered) where
  bmap f (FunctionTypeN ts t) = FunctionTypeN (fmap (fmap (bmap f) . f) <$> f ts) (bmap f <$> f t)

instance BareB (FunctionType n 'Uncurried) where
  bstrip (FunctionTypeN (Identity ts) (Identity t)) = FunctionTypeN (bstrip . runIdentity <$> ts) (bstrip t)
  bcover (FunctionTypeN ts t) = FunctionTypeN (Identity $ Identity . bcover <$> ts) (Identity $ bcover t)

instance
  ( Pretty (B.Wear b f [B.Wear b f (Type n 'Uncurried b f)])
  , Pretty (B.Wear b f (Type n 'Uncurried b f))
  ) =>
  Pretty (FunctionType n 'Uncurried b f) where

  pretty (FunctionTypeN ts t) = "(function " <> pretty ts <> " " <> pretty t <> ")"

-- Value

type Value :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family Value

type ValueConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family ValueConstraint cls n c l t b f where
  ValueConstraint cls n c l 'Untyped b f =
    ( cls (B.Wear b f (Value' n c l 'Untyped b f))
    , cls (B.Wear b f (ReferenceIdentifier n))
    , cls (B.Wear b f (BindIdentifier n))
    , cls (B.Wear b f Literal)
    )

  ValueConstraint cls n c l 'Typed b f =
    ( cls (B.Wear b f (Value' n c l 'Typed b f))
    , cls (B.Wear b f (ReferenceIdentifier n))
    , cls (B.Wear b f (BindIdentifier n))
    , cls (B.Wear b f Literal)
    , cls (B.Wear b f (Type n c b f))
    )

newtype instance Value n c l 'Untyped b f =
  UntypedValue (B.Wear b f (Value' n c l 'Untyped b f))
  deriving Generic

deriving instance ValueConstraint Show n c l 'Untyped b f => Show (Value n c l 'Untyped b f)
deriving instance ValueConstraint Eq n c l 'Untyped b f => Eq (Value n c l 'Untyped b f)
deriving instance ValueConstraint Ord n c l 'Untyped b f => Ord (Value n c l 'Untyped b f)

instance
  ( FunctorB (Function n c l 'Untyped B.Covered)
  , FunctorB (Application n c l 'Untyped B.Covered)
  , FunctorB (FunctionType n c B.Covered)
  , FunctorB (ValueBind n c l 'Untyped B.Covered)
  ) =>
  FunctorB (Value n c l 'Untyped B.Covered) where

  bmap f (UntypedValue v) = UntypedValue (bmap f <$> f v)

instance
  ( BareB (Application n c l 'Untyped)
  , BareB (ProcedureStep n c l 'Untyped)
  , BareB (TypeAnnotation n c l 'Untyped)
  , BareB (Definition n c l 'Untyped)
  , BareB (Function n c l 'Untyped)
  , FunctorB (ValueBind n c l 'Untyped B.Covered)
  , FunctorB (FunctionType n c B.Covered)
  ) =>
  BareB (Value n c l 'Untyped) where

  bstrip (UntypedValue (Identity v)) = UntypedValue $ bstrip v
  bcover (UntypedValue v) = UntypedValue $ Identity $ bcover v

data instance Value n c l 'Typed b f =
  TypedValue (B.Wear b f (Value' n c l 'Typed b f)) (B.Wear b f (Type n c b f))
  deriving Generic

deriving instance ValueConstraint Show n c l 'Typed b f => Show (Value n c l 'Typed b f)
deriving instance ValueConstraint Eq n c l 'Typed b f => Eq (Value n c l 'Typed b f)
deriving instance ValueConstraint Ord n c l 'Typed b f => Ord (Value n c l 'Typed b f)

instance
  ( FunctorB (Function n c l 'Typed B.Covered)
  , FunctorB (Application n c l 'Typed B.Covered)
  , FunctorB (FunctionType n c B.Covered)
  , FunctorB (ValueBind n c l 'Typed B.Covered)
  ) =>
  FunctorB (Value n c l 'Typed B.Covered) where

  bmap f (TypedValue v t) = TypedValue (bmap f <$> f v) (bmap f <$> f t)

instance
  ( BareB (FunctionType n c)
  , BareB (Application n c l 'Typed)
  , BareB (ProcedureStep n c l 'Typed)
  , BareB (TypeAnnotation n c l 'Typed)
  , BareB (Definition n c l 'Typed)
  , BareB (Function n c l 'Typed)
  , FunctorB (ValueBind n c l 'Typed B.Covered)
  ) =>
  BareB (Value n c l 'Typed) where

  bstrip (TypedValue (Identity v) (Identity t)) = TypedValue (bstrip v) (bstrip t)
  bcover (TypedValue v t) = TypedValue (Identity $ bcover v) (Identity $ bcover t)

-- Value'

data Value' n c l t b f
  = Variable (B.Wear b f (ReferenceIdentifier n))
  | Literal Literal
  | Function (Function n c l t b f)
  | Application (Application n c l t b f)
  | Procedure (B.Wear b f (NonEmpty (B.Wear b f (ProcedureStep n c l t b f))))
  | TypeAnnotation (TypeAnnotation n c l t b f)
  | Let (B.Wear b f [B.Wear b f (Definition n c l t b f)]) (B.Wear b f (Value n c l t b f))
  deriving Generic

type Value'Constraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type Value'Constraint cls n c l t b f =
  ( cls (B.Wear b f (ReferenceIdentifier n))
  , cls (B.Wear b f (BindIdentifier n))
  , cls (Application n c l t b f)
  , cls (B.Wear b f (ProcedureStep n c l t b f))
  , cls (B.Wear b f (NonEmpty (B.Wear b f (ProcedureStep n c l t b f))))
  , cls (TypeAnnotation n c l t b f)
  , cls (B.Wear b f (ValueBind n c l t b f))
  , cls (B.Wear b f (Value n c l t b f))
  , cls (B.Wear b f (Type n c b f))
  , cls (B.Wear b f (Definition n c l t b f))
  , cls (B.Wear b f [B.Wear b f (Definition n c l t b f)])
  , cls (B.Wear b f (ProcedureStep n c l t b f))
  , cls (Function n c l t b f)
  )

deriving instance Value'Constraint Show n c l t b f => Show (Value' n c l t b f)
deriving instance Value'Constraint Eq n c l t b f => Eq (Value' n c l t b f)
deriving instance Value'Constraint Ord n c l t b f => Ord (Value' n c l t b f)

instance
  ( FunctorB (Function n c l t B.Covered)
  , FunctorB (Application n c l t B.Covered)
  , FunctorB (TypeAnnotation n c l t B.Covered)
  , FunctorB (Value n c l t B.Covered)
  , FunctorB (FunctionType n c B.Covered)
  , FunctorB (ValueBind n c l t B.Covered)
  ) =>
  FunctorB (Value' n c l t B.Covered) where

  bmap f (Variable i)       = Variable $ f i
  bmap _ (Literal l)        = Literal l
  bmap f (Function v)       = Function $ bmap f v
  bmap f (Application a)    = Application $ bmap f a
  bmap f (Procedure ps)     = Procedure $ fmap (fmap (bmap f) . f) <$> f ps
  bmap f (TypeAnnotation a) = TypeAnnotation $ bmap f a
  bmap f (Let ds v)         = Let (fmap (fmap (bmap f) . f) <$> f ds) $ bmap f <$> f v

instance
  ( BareB (Application n c l t)
  , BareB (ProcedureStep n c l t)
  , BareB (TypeAnnotation n c l t)
  , BareB (Value n c l t)
  , BareB (Definition n c l t)
  , BareB (Function n c l t)
  , FunctorB (ValueBind n c l t B.Covered)
  , FunctorB (FunctionType n c B.Covered)
  ) =>
  BareB (Value' n c l t) where

  bstrip (Variable (Identity i))          = Variable i
  bstrip (Literal l)                      = Literal l
  bstrip (Function f)                     = Function $ bstrip f
  bstrip (Application a)                  = Application $ bstrip a
  bstrip (Procedure (Identity ss))        = Procedure $ bstrip . runIdentity <$> ss
  bstrip (TypeAnnotation a)               = TypeAnnotation $ bstrip a
  bstrip (Let (Identity ds) (Identity v)) = Let (bstrip . runIdentity <$> ds) (bstrip v)

  bcover (Variable i)       = Variable $ Identity i
  bcover (Literal l)        = Literal l
  bcover (Function f)       = Function $ bcover f
  bcover (Application a)    = Application $ bcover a
  bcover (Procedure ss)     = Procedure $ Identity $ Identity . bcover <$> ss
  bcover (TypeAnnotation a) = TypeAnnotation $ bcover a
  bcover (Let ds v)         = Let (Identity $ Identity . bcover <$> ds) (Identity $ bcover v)

-- TypeAnnotation

type TypeAnnotation :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family TypeAnnotation

data instance TypeAnnotation n c l 'Untyped b f =
  TypeAnnotation' (B.Wear b f (Value n c l 'Untyped b f)) (B.Wear b f (Type n c b f))
  deriving Generic

type TypeAnnotationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type TypeAnnotationConstraint cls n c l t b f =
  ( cls (B.Wear b f (ReferenceIdentifier n))
  , cls (B.Wear b f (BindIdentifier n))
  , cls (B.Wear b f (Function n c l t b f))
  , cls (B.Wear b f (Application n c l t b f))
  , cls (B.Wear b f (ProcedureStep n c l t b f))
  , cls (B.Wear b f (ValueBind n c l t b f))
  , cls (B.Wear b f (FunctionType n c b f))
  , cls (B.Wear b f (Value n c l t b f))
  , cls (B.Wear b f (Type n c b f))
  )

deriving instance TypeAnnotationConstraint Show n c l 'Untyped b f => Show (TypeAnnotation n c l 'Untyped b f)
deriving instance TypeAnnotationConstraint Eq n c l 'Untyped b f => Eq (TypeAnnotation n c l 'Untyped b f)
deriving instance TypeAnnotationConstraint Ord n c l 'Untyped b f => Ord (TypeAnnotation n c l 'Untyped b f)

instance
  ( FunctorB (Function n c l 'Untyped B.Covered)
  , FunctorB (Application n c l 'Untyped B.Covered)
  , FunctorB (FunctionType n c B.Covered)
  , FunctorB (ValueBind n c l 'Untyped B.Covered)
  ) =>
  FunctorB (TypeAnnotation n c l 'Untyped B.Covered) where

  bmap f (TypeAnnotation' v t) = TypeAnnotation' (bmap f <$> f v) (bmap f <$> f t)

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

instance FunctorB (TypeAnnotation n c l 'Typed B.Covered) where
  bmap _ = X.unreachable

instance BareB (TypeAnnotation n c l 'Typed) where
  bstrip = X.unreachable
  bcover = X.unreachable

-- ProcedureStep

data ProcedureStep n c l t b f
  = BindProcedure (B.Wear b f (BindIdentifier n)) (B.Wear b f (Value n c l t b f))
  | TermProcedure (B.Wear b f (Value n c l t b f))

type ProcedureStepConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type ProcedureStepConstraint cls n c l t b f =
  ( cls (B.Wear b f (BindIdentifier n))
  , cls (B.Wear b f (Value n c l t b f))
  )

deriving instance ProcedureStepConstraint Show n c l t b f => Show (ProcedureStep n c l t b f)
deriving instance ProcedureStepConstraint Eq n c l t b f => Eq (ProcedureStep n c l t b f)
deriving instance ProcedureStepConstraint Ord n c l t b f => Ord (ProcedureStep n c l t b f)

instance FunctorB (Value n c l t B.Covered) => FunctorB (ProcedureStep n c l t B.Covered) where
  bmap f (BindProcedure i v) = BindProcedure (f i) (bmap f <$> f v)
  bmap f (TermProcedure v)   = TermProcedure (bmap f <$> f v)

instance (BareB (Value n c l t)) => BareB (ProcedureStep n c l t) where
  bstrip (BindProcedure (Identity i) (Identity v)) = BindProcedure i (bstrip v)
  bstrip (TermProcedure (Identity v))              = TermProcedure (bstrip v)

  bcover (BindProcedure i v) = BindProcedure (Identity i) (Identity $ bcover v)
  bcover (TermProcedure v)   = TermProcedure (Identity $ bcover v)

-- Literal

data Literal
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)

-- Application

type Application :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family Application

type ApplicationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family ApplicationConstraint cls n c l t b f where
  ApplicationConstraint cls n 'Curried l t b f =
    (cls (B.Wear b f (Value n 'Curried l t b f)))

  ApplicationConstraint cls n 'Uncurried l t b f =
    ( cls (B.Wear b f (Value n 'Uncurried l t b f))
    , cls (B.Wear b f [B.Wear b f (Value n 'Uncurried l t b f)])
    )

data instance Application n 'Curried l t b f =
  ApplicationC (B.Wear b f (Value n 'Curried l t b f)) (B.Wear b f (Value n 'Curried l t b f))
  deriving Generic

deriving instance ApplicationConstraint Show n 'Curried l t b f => Show (Application n 'Curried l t b f)
deriving instance ApplicationConstraint Eq n 'Curried l t b f => Eq (Application n 'Curried l t b f)
deriving instance ApplicationConstraint Ord n 'Curried l t b f => Ord (Application n 'Curried l t b f)

instance FunctorB (Value n 'Curried l t B.Covered) => FunctorB (Application n 'Curried l t B.Covered) where
  bmap f (ApplicationC v1 v2) = ApplicationC (bmap f <$> f v1) (bmap f <$> f v2)

instance BareB (Value n 'Curried l t) => BareB (Application n 'Curried l t) where
  bstrip (ApplicationC (Identity v1) (Identity v2)) = ApplicationC (bstrip v1) (bstrip v2)
  bcover (ApplicationC v1 v2) = ApplicationC (Identity $ bcover v1) (Identity $ bcover v2)

data instance Application n 'Uncurried l t b f =
  ApplicationN (B.Wear b f (Value n 'Uncurried l t b f)) (B.Wear b f [B.Wear b f (Value n 'Uncurried l t b f)])
  deriving Generic

deriving instance ApplicationConstraint Show n 'Uncurried l t b f => Show (Application n 'Uncurried l t b f)
deriving instance ApplicationConstraint Eq n 'Uncurried l t b f => Eq (Application n 'Uncurried l t b f)
deriving instance ApplicationConstraint Ord n 'Uncurried l t b f => Ord (Application n 'Uncurried l t b f)

instance FunctorB (Value n 'Uncurried l t B.Covered) => FunctorB (Application n 'Uncurried l t B.Covered) where
  bmap f (ApplicationN v1 vs) = ApplicationN (bmap f <$> f v1) $ fmap (fmap (bmap f) . f) <$> f vs

instance BareB (Value n 'Uncurried l t) => BareB (Application n 'Uncurried l t) where
  bstrip (ApplicationN (Identity v) (Identity vs)) = ApplicationN (bstrip v) (bstrip . runIdentity <$> vs)
  bcover (ApplicationN v vs) = ApplicationN (Identity $ bcover v) (Identity $ Identity . bcover <$> vs)

-- Function

type Function :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Type
data family Function

type FunctionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type -> (K.Type -> K.Type) -> K.Constraint
type family FunctionConstraint cls n c l t b f where
  FunctionConstraint cls n 'Curried l t b f =
    ( cls (B.Wear b f (BindIdentifier n))
    , cls (B.Wear b f (ReferenceIdentifier n))
    , cls (B.Wear b f (Value n 'Curried l t b f))
    , cls (B.Wear b f (Type n 'Curried b f))
    )

  FunctionConstraint cls n 'Uncurried l t b f =
    ( cls (B.Wear b f (BindIdentifier n))
    , cls (B.Wear b f (ReferenceIdentifier n))
    , cls (B.Wear b f (Value n 'Uncurried l t b f))
    , cls (B.Wear b f (Type n 'Uncurried b f))
    , cls (B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f (Type n 'Uncurried b f))])
    )

data instance Function n 'Curried 'LambdaUnlifted t b f =
  FunctionC (B.Wear b f (BindIdentifier n)) (B.Wear b f (Type n 'Curried b f)) (B.Wear b f (Value n 'Curried 'LambdaUnlifted t b f))
  deriving Generic

deriving instance FunctionConstraint Show n 'Curried 'LambdaUnlifted t b f => Show (Function n 'Curried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Eq n 'Curried 'LambdaUnlifted t b f => Eq (Function n 'Curried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Ord n 'Curried 'LambdaUnlifted t b f => Ord (Function n 'Curried 'LambdaUnlifted t b f)

instance FunctorB (Value n 'Curried 'LambdaUnlifted t B.Covered) => FunctorB (Function n 'Curried 'LambdaUnlifted t B.Covered) where
  bmap f (FunctionC i t v) = FunctionC (f i) (bmap f <$> f t) (bmap f <$> f v)

instance BareB (Value n 'Curried 'LambdaUnlifted t) => BareB (Function n 'Curried 'LambdaUnlifted t) where
  bstrip (FunctionC (Identity i) (Identity t) (Identity v)) = FunctionC i (bstrip t) (bstrip v)
  bcover (FunctionC i t v) = FunctionC (Identity i) (Identity $ bcover t) (Identity $ bcover v)

data instance Function n 'Uncurried 'LambdaUnlifted t b f =
  FunctionN (B.Wear b f [B.Wear b f (B.Wear b f (BindIdentifier n), B.Wear b f (Type n 'Uncurried b f))]) (B.Wear b f (Value n 'Uncurried 'LambdaUnlifted t b f))
  deriving Generic

deriving instance FunctionConstraint Show n 'Uncurried 'LambdaUnlifted t b f => Show (Function n 'Uncurried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Eq n 'Uncurried 'LambdaUnlifted t b f => Eq (Function n 'Uncurried 'LambdaUnlifted t b f)
deriving instance FunctionConstraint Ord n 'Uncurried 'LambdaUnlifted t b f => Ord (Function n 'Uncurried 'LambdaUnlifted t b f)

instance FunctorB (Value n 'Uncurried 'LambdaUnlifted t B.Covered) => FunctorB (Function n 'Uncurried 'LambdaUnlifted t B.Covered) where
  bmap f (FunctionN ps v) = FunctionN (fmap (fmap (bimap f (fmap (bmap f) . f)) . f) <$> f ps) (bmap f <$> f v)

instance BareB (Value n 'Uncurried 'LambdaUnlifted t) => BareB (Function n 'Uncurried 'LambdaUnlifted t) where
  bstrip (FunctionN (Identity ps) (Identity v)) = FunctionN (bimap runIdentity (bstrip . runIdentity) . runIdentity <$> ps) (bstrip v)
  bcover (FunctionN ps v) = FunctionN (Identity $ Identity . bimap Identity (Identity . bcover) <$> ps) (Identity $ bcover v)

data instance Function n c 'LambdaLifted t b f deriving (Show, Read, Eq, Ord, Generic)

instance FunctorB (Function n c 'LambdaLifted t B.Covered) where
  bmap _ = X.unreachable

instance BareB (Function n c 'LambdaLifted t) where
  bstrip = X.unreachable
  bcover = X.unreachable

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

-- Positions

class HasPosition f where
  range :: f a -> Maybe (Position, Position)

-- | A position in a source file.
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

instance Pretty a => Pretty (WithPosition a) where
  pretty (WithPosition _ _ a) = pretty a

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

strip :: (Functor f, Copointed f, BareB b) => f (b B.Covered f) -> b B.Bare Identity
strip = bstripFrom copoint . copoint

-- Pretty

-- | A pretty-printable class.
class Pretty a where
  pretty :: a -> Text

instance Pretty a => Pretty [a] where
  pretty as = "(list " <> T.intercalate " " (pretty <$> as) <> ")"
