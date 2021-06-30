{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
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
    -- * Kinds and types
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  , NameResolving (..)
  ) where

import           Data.Function         (on)
import qualified Data.Kind             as K
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as N
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import           GHC.Exts              (IsList)
import qualified GHC.Exts              as E
import           GHC.Generics          (Generic)
import qualified Language.C.Pretty     as C
import           Language.C.Syntax.AST (CExtDecl)
import qualified Text.PrettyPrint      as P

data Module n c l t =
  Module ModuleName [ModuleName] [Definition n c l t]
  deriving Generic

deriving instance Show (Definition n c l t) => Show (Module n c l t)
deriving instance Read (Definition n c l t) => Read (Module n c l t)
deriving instance Eq (Definition n c l t) => Eq (Module n c l t)
deriving instance Ord (Definition n c l t) => Ord (Module n c l t)

data Definition n c l t
  = DataDefinition (BindIdentifier n) [(BindIdentifier n, [(BindIdentifier n, Type n c)])]
  | TypeBind (BindIdentifier n) (Type n c)
  | ValueBind (ValueBind n c l t)
  | ForeignTypeBind (BindIdentifier n) [CHeader] CDefinition
  | ForeignValueBind (BindIdentifier n) [CHeader] CDefinition (Type n c)
  deriving Generic

type DefinitionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type DefinitionConstraint cls n c l t =
  ( cls (BindIdentifier n)
  , cls (ValueBind n c l t)
  , cls (Type n c)
  )

deriving instance DefinitionConstraint Show n c l t => Show (Definition n c l t)
deriving instance DefinitionConstraint Eq n c l t => Eq (Definition n c l t)
deriving instance DefinitionConstraint Ord n c l t => Ord (Definition n c l t)

type ValueBind :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type
data family ValueBind

data instance ValueBind n c 'LambdaUnlifted t =
  ValueBindU (BindIdentifier n) (Value n c 'LambdaUnlifted t)
  deriving Generic

data instance ValueBind n c 'LambdaLifted t
  = ValueBindV (BindIdentifier n) (Value n c 'LambdaLifted t)
  | ValueBindN (BindIdentifier n) [(BindIdentifier n, Type n c)] (Value n c 'LambdaLifted t)

type ValueBindUnliftedConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type ValueBindUnliftedConstraint cls n c l t =
  ( cls (BindIdentifier n)
  , cls (Value n c l t)
  )

type ValueBindLiftedConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type ValueBindLiftedConstraint cls n c l t =
  ( cls (BindIdentifier n)
  , cls (Value n c l t)
  , cls (Type n c)
  )

deriving instance ValueBindUnliftedConstraint Show n c 'LambdaUnlifted t => Show (ValueBind n c 'LambdaUnlifted t)
deriving instance ValueBindUnliftedConstraint Read n c 'LambdaUnlifted t => Read (ValueBind n c 'LambdaUnlifted t)
deriving instance ValueBindUnliftedConstraint Eq n c 'LambdaUnlifted t => Eq (ValueBind n c 'LambdaUnlifted t)
deriving instance ValueBindUnliftedConstraint Ord n c 'LambdaUnlifted t => Ord (ValueBind n c 'LambdaUnlifted t)

deriving instance ValueBindLiftedConstraint Show n c 'LambdaLifted t => Show (ValueBind n c 'LambdaLifted t)
deriving instance ValueBindLiftedConstraint Read n c 'LambdaLifted t => Read (ValueBind n c 'LambdaLifted t)
deriving instance ValueBindLiftedConstraint Eq n c 'LambdaLifted t => Eq (ValueBind n c 'LambdaLifted t)
deriving instance ValueBindLiftedConstraint Ord n c 'LambdaLifted t => Ord (ValueBind n c 'LambdaLifted t)

newtype CDefinition = CDefinition CExtDecl deriving (Show, Generic)

instance Eq CDefinition where
  (==) = (==) `on` (\(CDefinition c) -> P.render $ C.pretty c)

instance Ord CDefinition where
  compare = compare `on` (\(CDefinition c) -> P.render $ C.pretty c)

data CHeader
  = SystemHeader Text
  | LocalHeader Text
  deriving (Show, Read, Eq, Ord, Generic)

type Type :: NameResolving -> Currying -> K.Type
data Type n c
  = TypeVariable (ReferenceIdentifier n)
  | TypeApplication (Type n c) (Type n c)
  | FunctionType (FunctionType n c)
  | ProcedureType (Type n c)
  deriving Generic

type TypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> K.Constraint
type TypeConstraint cls n c =
  ( cls (ReferenceIdentifier n)
  , cls (FunctionType n c)
  )

deriving instance TypeConstraint Show n c => Show (Type n c)
deriving instance TypeConstraint Read n c => Read (Type n c)
deriving instance TypeConstraint Eq n c => Eq (Type n c)
deriving instance TypeConstraint Ord n c => Ord (Type n c)

type FunctionType :: NameResolving -> Currying -> K.Type
data family FunctionType

data instance FunctionType n 'Curried =
  FunctionTypeC (Type n 'Curried) (Type n 'Curried)
  deriving Generic

data instance FunctionType n 'Uncurried =
  FunctionTypeN [Type n 'Uncurried] (Type n 'Uncurried)
  deriving Generic

type FunctionTypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> K.Constraint
type FunctionTypeConstraint cls n c =
  ( cls (ReferenceIdentifier n)
  )

deriving instance FunctionTypeConstraint Show n 'Curried => Show (FunctionType n 'Curried)
deriving instance FunctionTypeConstraint Read n 'Curried => Read (FunctionType n 'Curried)
deriving instance FunctionTypeConstraint Eq n 'Curried => Eq (FunctionType n 'Curried)
deriving instance FunctionTypeConstraint Ord n 'Curried => Ord (FunctionType n 'Curried)

deriving instance FunctionTypeConstraint Show n 'Uncurried => Show (FunctionType n 'Uncurried)
deriving instance FunctionTypeConstraint Read n 'Uncurried => Read (FunctionType n 'Uncurried)
deriving instance FunctionTypeConstraint Eq n 'Uncurried => Eq (FunctionType n 'Uncurried)
deriving instance FunctionTypeConstraint Ord n 'Uncurried => Ord (FunctionType n 'Uncurried)

type Value :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type
data family Value

data instance Value n c l 'Typed =
  TypedTerm (Value' n c l 'Typed) (Type n c)
  deriving Generic

newtype instance Value n c l 'Untyped =
  UntypedValue (Value' n c l 'Untyped)
  deriving Generic

type ValueConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type ValueConstraint cls n c l t =
  ( cls (Value' n c l t)
  , cls (ReferenceIdentifier n)
  , cls (Type n c)
  , cls (BindIdentifier n)
  , cls (Literal n c l t)
  )

deriving instance ValueConstraint Show n c l 'Untyped => Show (Value n c l 'Untyped)
deriving instance ValueConstraint Read n c l 'Untyped => Read (Value n c l 'Untyped)
deriving instance ValueConstraint Eq n c l 'Untyped => Eq (Value n c l 'Untyped)
deriving instance ValueConstraint Ord n c l 'Untyped => Ord (Value n c l 'Untyped)

deriving instance ValueConstraint Show n c l 'Typed => Show (Value n c l 'Typed)
deriving instance ValueConstraint Read n c l 'Typed => Read (Value n c l 'Typed)
deriving instance ValueConstraint Eq n c l 'Typed => Eq (Value n c l 'Typed)
deriving instance ValueConstraint Ord n c l 'Typed => Ord (Value n c l 'Typed)

data Value' n c l t
  = Variable (ReferenceIdentifier n)
  | Literal (Literal n c l t)
  | Application (Application n c l t)
  | Procedure (NonEmpty (ProcedureStep n c l t))
  | TypeAnnotation (TypeAnnotation n c l t)
  | Let [Definition n c l t] (Value n c l t)
  deriving Generic

type Value'Constraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type Value'Constraint cls n c l t =
  ( cls (ReferenceIdentifier n)
  , cls (BindIdentifier n)
  , cls (Literal n c l t)
  , cls (Application n c l t)
  , cls (ProcedureStep n c l t)
  , cls (TypeAnnotation n c l t)
  , cls (ValueBind n c l t)
  , cls (Value n c l t)
  , cls (Type n c)
  )

deriving instance Value'Constraint Show n c l t => Show (Value' n c l t)
deriving instance Value'Constraint Eq n c l t => Eq (Value' n c l t)
deriving instance Value'Constraint Ord n c l t => Ord (Value' n c l t)

type TypeAnnotation :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type
data family TypeAnnotation

data instance TypeAnnotation n c l 'Untyped =
  TypeAnnotation' (Value n c l 'Untyped) (Type n c)

data instance TypeAnnotation n c l 'Typed

type TypeAnnotationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type TypeAnnotationConstraint cls n c l t =
  ( cls (ReferenceIdentifier n)
  , cls (BindIdentifier n)
  , cls (Function n c l t)
  , cls (Application n c l t)
  , cls (ProcedureStep n c l t)
  , cls (ValueBind n c l t)
  , cls (FunctionType n c)
  )

deriving instance TypeAnnotationConstraint Show n c l 'Untyped => Show (TypeAnnotation n c l 'Untyped)
deriving instance TypeAnnotationConstraint Eq n c l 'Untyped => Eq (TypeAnnotation n c l 'Untyped)
deriving instance TypeAnnotationConstraint Ord n c l 'Untyped => Ord (TypeAnnotation n c l 'Untyped)

deriving instance TypeAnnotationConstraint Show n c l 'Typed => Show (TypeAnnotation n c l 'Typed)
deriving instance TypeAnnotationConstraint Eq n c l 'Typed => Eq (TypeAnnotation n c l 'Typed)
deriving instance TypeAnnotationConstraint Ord n c l 'Typed => Ord (TypeAnnotation n c l 'Typed)

data ProcedureStep n c l t
  = BindProcedure (BindIdentifier n) (Value n c l t)
  | TermProcedure (Value n c l t)

type ProcedureStepConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type ProcedureStepConstraint cls n c l t =
  ( cls (BindIdentifier n)
  , cls (Value n c l t)
  )

deriving instance ProcedureStepConstraint Show n c l t => Show (ProcedureStep n c l t)
deriving instance ProcedureStepConstraint Eq n c l t => Eq (ProcedureStep n c l t)
deriving instance ProcedureStepConstraint Ord n c l t => Ord (ProcedureStep n c l t)

data Literal n c l t
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function (Function n c l t)
  deriving Generic

type LiteralConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type LiteralConstraint cls n c l t = cls (Function n c l t)

deriving instance LiteralConstraint Show n c l t => Show (Literal n c l t)
deriving instance LiteralConstraint Eq n c l t => Eq (Literal n c l t)
deriving instance LiteralConstraint Ord n c l t => Ord (Literal n c l t)

type Application :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type
data family Application

data instance Application n 'Curried l t =
  ApplicationC (Value n 'Curried l t) (Value n 'Curried l t)
  deriving Generic

data instance Application n 'Uncurried l t =
  ApplicationN (Value n 'Uncurried l t) [Value n 'Uncurried l t]
  deriving Generic

type ApplicationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type ApplicationConstraint cls n c l t = (cls (Value n c l t))

deriving instance ApplicationConstraint Show n 'Curried l t => Show (Application n 'Curried l t)
deriving instance ApplicationConstraint Eq n 'Curried l t => Eq (Application n 'Curried l t)
deriving instance ApplicationConstraint Ord n 'Curried l t => Ord (Application n 'Curried l t)

deriving instance ApplicationConstraint Show n 'Uncurried l t => Show (Application n 'Uncurried l t)
deriving instance ApplicationConstraint Eq n 'Uncurried l t => Eq (Application n 'Uncurried l t)
deriving instance ApplicationConstraint Ord n 'Uncurried l t => Ord (Application n 'Uncurried l t)

type Function :: NameResolving -> Currying -> LambdaLifting -> Typing -> K.Type
data family Function

data instance Function n 'Curried 'LambdaUnlifted t =
  FunctionC (BindIdentifier n) (Type n 'Curried) (Value n 'Curried 'LambdaUnlifted t)
  deriving Generic

data instance Function n 'Uncurried 'LambdaUnlifted t =
  FunctionN [(BindIdentifier n, Type n 'Uncurried)] (Value n 'Uncurried 'LambdaUnlifted t)
  deriving Generic

data instance Function n c 'LambdaLifted t deriving (Show, Read, Eq, Ord, Generic)

type FunctionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> K.Constraint
type FunctionConstraint cls n c l t =
  ( cls (BindIdentifier n)
  , cls (ReferenceIdentifier n)
  , cls (Value n c l t)
  )

deriving instance FunctionConstraint Show n 'Curried 'LambdaUnlifted t => Show (Function n 'Curried 'LambdaUnlifted t)
deriving instance FunctionConstraint Eq n 'Curried 'LambdaUnlifted t => Eq (Function n 'Curried 'LambdaUnlifted t)
deriving instance FunctionConstraint Ord n 'Curried 'LambdaUnlifted t => Ord (Function n 'Curried 'LambdaUnlifted t)

deriving instance FunctionConstraint Show n 'Uncurried 'LambdaUnlifted t => Show (Function n 'Uncurried 'LambdaUnlifted t)
deriving instance FunctionConstraint Eq n 'Uncurried 'LambdaUnlifted t => Eq (Function n 'Uncurried 'LambdaUnlifted t)
deriving instance FunctionConstraint Ord n 'Uncurried 'LambdaUnlifted t => Ord (Function n 'Uncurried 'LambdaUnlifted t)

data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = UserIdentifier . fromString

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

type BindIdentifier :: NameResolving -> K.Type
type family BindIdentifier n

type instance BindIdentifier 'NameUnresolved = Identifier

type instance BindIdentifier 'NameResolved = QualifiedIdentifier

type ReferenceIdentifier :: NameResolving -> K.Type
type family ReferenceIdentifier n

type instance ReferenceIdentifier 'NameUnresolved = EitherIdentifier

type instance ReferenceIdentifier 'NameResolved = QualifiedIdentifier

newtype ModuleName = ModuleName (N.NonEmpty Text) deriving (Show, Read, Eq, Ord, Generic)

instance IsString ModuleName where
  fromString = ModuleName . (:| []) . fromString

instance IsList ModuleName where
  type Item ModuleName = Text
  fromList = ModuleName . N.fromList
  toList (ModuleName n) = N.toList n

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
