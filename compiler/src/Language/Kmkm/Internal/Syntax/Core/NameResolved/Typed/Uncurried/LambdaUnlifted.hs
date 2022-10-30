{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted
  ( -- * Modules and definitions
    Module (..)
  , Definition (..)
  , DataRepresentation (..)
  , ValueConstructor (..)
  , Field (..)
    -- * Types
  , Type (..)
    -- * Values
  , Value (..)
  , Value' (..)
  , ProcedureStep (..)
  , Literal (..)
  , EmbeddedValue (..)
  , EmbeddedCValue (..)
  , isEmbeddedCValue
  , EmbeddedType (..)
  , EmbeddedCType (..)
  , isEmbeddedCType
    -- * Identifiers
  , BindIdentifier
  , ReferenceIdentifier
  ) where

import qualified Language.Kmkm.Internal.Exception                          as X
import           Language.Kmkm.Internal.Syntax.Core.Common                 (Constraint, EmbeddedCType (EmbeddedCType),
                                                                            EmbeddedCValue (EmbeddedCValue),
                                                                            EmbeddedType (EmbeddedTypeC),
                                                                            EmbeddedValue (EmbeddedValueC),
                                                                            Literal (Fraction, Integer, String, base, exponent, fractionDigits, significand, value),
                                                                            ModuleName (ModuleName), Pretty (pretty),
                                                                            Pretty1,
                                                                            QualifiedIdentifier (GlobalIdentifier),
                                                                            isEmbeddedCType, isEmbeddedCValue,
                                                                            prettyList)
import           Language.Kmkm.Internal.Syntax.Core.NameResolved           (BindIdentifier, ReferenceIdentifier)
import           Language.Kmkm.Internal.Syntax.Core.NameResolved.Uncurried (Type (ForAllType, FunctionType, ProcedureType, TypeApplication, TypeVariable))

import           Data.Bifunctor              (Bifunctor (bimap))
import           Data.Copointed              (Copointed (copoint))
import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Classes        (Eq1, Ord1, Show1)
import           Data.Functor.F              (F)
import qualified Data.Functor.F              as F
import qualified Data.Kind                   as K
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as N
import           Data.Pointed                (Pointed (point))
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import           GHC.Exts                    (IsList)
import qualified GHC.Exts                    as E
import           GHC.Generics                (Generic)

-- Module

type Module :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Module et ev f =
  Module (F f ModuleName) (F f [F f ModuleName]) (F f [F f (Definition et ev f)])
  deriving Generic

deriving instance Constraint Show Show1 et ev f => Show (Module et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (Module et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (Module et ev f)

instance (FunctorB et, FunctorB ev) => FunctorB (Module et ev) where
  bmap f (Module n ms ds) = Module (F.map f n) (fmap (F.map f) <$> F.map f ms) (fmap (fmap (bmap f) . F.map f) <$> F.map f ds)

instance Constraint Pretty Pretty1 et ev f => Pretty (Module et ev f) where
  pretty (Module n is ds) = prettyList "module" [pretty n, pretty is, pretty ds]

-- Definition

type Definition :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Definition et ev f
  = DataDefinition (F f BindIdentifier) (F f (DataRepresentation et ev f))
  | TypeBind (F f BindIdentifier) (F f (Type f))
  | ValueBind (F f BindIdentifier) (F f (Value et ev f))
  | ForeignTypeBind (F f BindIdentifier) (F f (et f))
  | ForeignValueBind (F f BindIdentifier) (F f (ev f)) (F f (Type f))
  deriving Generic

deriving instance Constraint Show Show1 et ev f => Show (Definition et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (Definition et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (Definition et ev f)

instance (FunctorB et, FunctorB ev) => FunctorB (Definition et ev) where
  bmap f (DataDefinition n r)     = DataDefinition (F.map f n) $ bmap f <$> F.map f r
  bmap f (TypeBind i t)           = TypeBind (F.map f i) (bmap f <$> F.map f t)
  bmap f (ValueBind i v)          = ValueBind (F.map f i) (bmap f <$> F.map f v)
  bmap f (ForeignTypeBind i c)    = ForeignTypeBind (F.map f i) (bmap f <$> F.map f c)
  bmap f (ForeignValueBind i c t) = ForeignValueBind (F.map f i) (bmap f <$> F.map f c) (bmap f <$> F.map f t)

instance Constraint Pretty Pretty1 et ev f => Pretty (Definition et ev f) where
  pretty (DataDefinition n r)     = prettyList "define" [pretty n, pretty r]
  pretty (TypeBind i t)           = prettyList "bind-type" [pretty i, pretty t]
  pretty (ValueBind i v)          = pretty i <> " " <> pretty v
  pretty (ForeignTypeBind i c)    = prettyList "bind-type-foreign" [pretty i, pretty c]
  pretty (ForeignValueBind i c t) = prettyList "bind-value-foreign" [pretty i, pretty c, pretty t]

-- DataRepresentation

type DataRepresentation :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data DataRepresentation et ev f = ForAllData (F f [F f BindIdentifier]) (F f [F f (ValueConstructor et ev f)])

deriving instance Constraint Show Show1 et ev f => Show (DataRepresentation et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (DataRepresentation et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (DataRepresentation et ev f)

instance FunctorB (DataRepresentation et ev) where
  bmap f (ForAllData is cs)           = ForAllData (fmap (F.map f) <$> F.map f is) $ fmap (fmap (bmap f) . F.map f) <$> F.map f cs

instance Pretty1 f => Pretty (DataRepresentation et ev f) where
  pretty (ForAllData is cs)          = prettyList "for-all" [pretty is, pretty cs]

-- ValueConstructor

type ValueConstructor :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data ValueConstructor et ev f = ValueConstructor (F f BindIdentifier) (F f [F f (Field et ev f)])

deriving instance Constraint Show Show1 et ev f => Show (ValueConstructor et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (ValueConstructor et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (ValueConstructor et ev f)

instance FunctorB (ValueConstructor et ev) where
  bmap f (ValueConstructor i fs) = ValueConstructor (F.map f i) (fmap (fmap (bmap f) . F.map f) <$> F.map f fs)

instance Pretty1 f => Pretty (ValueConstructor et ev f) where
  pretty (ValueConstructor i fs) =
    case pretty fs of
      "(list)" -> pretty i
      t        -> "(" <> pretty i <> " " <> t <> ")"

-- Field

type Field :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Field et ev f = Field (F f BindIdentifier) (F f (Type f))

deriving instance Constraint Show Show1 et ev f => Show (Field et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (Field et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (Field et ev f)

instance FunctorB (Field et ev) where
  bmap f (Field i t) = Field (F.map f i) (bmap f <$> F.map f t)

instance Pretty1 f => Pretty (Field et ev f) where
  pretty (Field i t) = "(" <> pretty i <> " " <> pretty t <> ")"

-- Value

type Value :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Value et ev f =
  TypedValue (F f (Value' et ev f)) (F f (Type f))
  deriving Generic

deriving instance Constraint Show Show1 et ev f => Show (Value et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (Value et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (Value et ev f)

instance (FunctorB et, FunctorB ev) => FunctorB (Value et ev) where
  bmap f (TypedValue v t) = TypedValue (bmap f <$> F.map f v) (bmap f <$> F.map f t)

instance Pointed f => Num (Value et ev f) where
  fromInteger = flip TypedValue (point $ TypeVariable $ point $ GlobalIdentifier (ModuleName $ "kmkm" N.:| ["prim"]) "int") . point . fromInteger
  (+) = X.unreachable "(+)"
  (*) = X.unreachable "(*)"
  abs = X.unreachable "abs"
  signum = X.unreachable "signum"
  negate = X.unreachable "negate"

instance Constraint Pretty Pretty1 et ev f => Pretty (Value et ev f) where
  pretty (TypedValue v t) = prettyList "type" [pretty v, pretty t]

-- Value'

type Value' :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Value' et ev f
  = Variable (F f ReferenceIdentifier)
  | Literal (F f Literal)
  | Function (F f [F f (F f BindIdentifier, F f (Type f))]) (F f (Value et ev f))
  | Application (F f (Value et ev f)) (F f [F f (Value et ev f)])
  | Procedure (F f (NonEmpty (F f (ProcedureStep et ev f))))
  | Let (F f [F f (Definition et ev f)]) (F f (Value et ev f))
  | ForAllValue (F f BindIdentifier) (F f (Value et ev f))
  | Instantiation (F f (Value et ev f)) (F f (Type f))
  deriving Generic

deriving instance Constraint Show Show1 et ev f => Show (Value' et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (Value' et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (Value' et ev f)

instance (FunctorB et, FunctorB ev) => FunctorB (Value' et ev) where
  bmap f (Variable i)         = Variable $ F.map f i
  bmap f (Literal l)          = Literal $ F.map f l
  bmap f (Function ps v)     = Function (fmap (fmap (bimap (F.map f) $ fmap (bmap f) . F.map f) . F.map f) <$> F.map f ps) (bmap f <$> F.map f v)
  bmap f (Application v vs)  = Application (bmap f <$> F.map f v) $ fmap (fmap (bmap f) . F.map f) <$> F.map f vs
  bmap f (Procedure ps)       = Procedure $ fmap (fmap (bmap f) . F.map f) <$> F.map f ps
  bmap f (Let ds v)           = Let (fmap (fmap (bmap f) . F.map f) <$> F.map f ds) $ bmap f <$> F.map f v
  bmap f (ForAllValue i v)    = ForAllValue (F.map f i) $ bmap f <$> F.map f v
  bmap f (Instantiation v t)  = Instantiation (bmap f <$> F.map f v) $ bmap f <$> F.map f t

instance Pointed f => IsString (Value' et ev f) where
  fromString = Variable . point . fromString

instance (E.Item ReferenceIdentifier ~ Text, Pointed f, Copointed f) => IsList (Value' et ev f) where
  type Item (Value' et ev f) = Text
  fromList = Variable . point . E.fromList
  toList (Variable i) = E.toList $ copoint i
  toList _            = error "only variable acceptable"

instance Pointed f => Num (Value' et ev f) where
  fromInteger = Literal . point . fromInteger
  (+) = X.unreachable "(+)"
  (*) = X.unreachable "(*)"
  abs = X.unreachable "abs"
  signum = X.unreachable "signum"
  negate = X.unreachable "negate"

instance Constraint Pretty Pretty1 et ev f => Pretty (Value' et ev f) where
  pretty (Variable n)         = pretty n
  pretty (Literal l)          = pretty l
  pretty (Function ps v)      = prettyList "function" [pretty ps, pretty v]
  pretty (Application v1 v2s) = prettyList "apply" [pretty v1, pretty v2s]
  pretty (Procedure ps)       = prettyList "procedure" [pretty ps]
  -- pretty (TypeAnnotation v t) = prettyList "type" [pretty v, pretty t]
  pretty (Let ds v)           = prettyList "let" [pretty ds, pretty v]
  pretty (ForAllValue i v)    = prettyList "for-all" [pretty i, pretty v]
  pretty (Instantiation v t)  = prettyList "instantiate" [pretty v, pretty t]

-- ProcedureStep

data ProcedureStep et ev f
  = BindProcedureStep (F f BindIdentifier) (F f (Value et ev f))
  | CallProcedureStep (F f (Value et ev f))

deriving instance Constraint Show Show1 et ev f => Show (ProcedureStep et ev f)
deriving instance Constraint Eq Eq1 et ev f => Eq (ProcedureStep et ev f)
deriving instance Constraint Ord Ord1 et ev f => Ord (ProcedureStep et ev f)

instance (FunctorB et, FunctorB ev) => FunctorB (ProcedureStep et ev) where
  bmap f (BindProcedureStep i v) = BindProcedureStep (F.map f i) (bmap f <$> F.map f v)
  bmap f (CallProcedureStep v)   = CallProcedureStep (bmap f <$> F.map f v)

instance Constraint Pretty Pretty1 et ev f => Pretty (ProcedureStep et ev f) where
  pretty (BindProcedureStep n v) = prettyList "bind" [pretty n, pretty v]
  pretty (CallProcedureStep v)   = prettyList "call" [pretty v]
