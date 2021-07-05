{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
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
  , Item (..)
  , Metadata (..)
  , HasPosition (..)
  , Position (..)
  , AttachPosition (..)
    -- * Kinds and types
  , Currying (..)
  , Typing (..)
  , LambdaLifting (..)
  , NameResolving (..)
    -- * Pretty printing
  , Pretty (..)
  ) where

import           Data.Foldable         (Foldable (fold))
import           Data.Function         (on)
import           Data.Functor.Identity (Identity (Identity))
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

type Module :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data Module n c l t f =
    Module (f ModuleName) [f ModuleName] [f (Definition n c l t f)]
  deriving Generic

type ModuleConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ModuleConstraint cls n c l t f =
  ( cls (f ModuleName)
  , cls (f (Definition n c l t f))
  )

deriving instance ModuleConstraint Show n c l t f => Show (Module n c l t f)
deriving instance ModuleConstraint Read n c l t f => Read (Module n c l t f)
deriving instance ModuleConstraint Eq n c l t f => Eq (Module n c l t f)
deriving instance ModuleConstraint Ord n c l t f => Ord (Module n c l t f)

instance Item (Module n c l t) where
  prune m =
    let Module n ms ds = item m
    in Identity $ Module (Identity $ item n) (Identity . item <$> ms) (prune <$> ds)

type Definition :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data Definition n c l t f
  = DataDefinition (f (BindIdentifier n)) [(f (BindIdentifier n), [(f (BindIdentifier n), f (Type n c f))])]
  | TypeBind (f (BindIdentifier n)) (f (Type n c f))
  | ValueBind (ValueBind n c l t f)
  | ForeignTypeBind (f (BindIdentifier n)) [CHeader] (f CDefinition)
  | ForeignValueBind (f (BindIdentifier n)) [CHeader] (f CDefinition) (f (Type n c f))
  deriving Generic

type DefinitionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type DefinitionConstraint cls n c l t f =
  ( cls (f (BindIdentifier n))
  , cls (ValueBind n c l t f)
  , cls (f (Type n c f))
  , cls (f CDefinition)
  )

deriving instance DefinitionConstraint Show n c l t f => Show (Definition n c l t f)
deriving instance DefinitionConstraint Eq n c l t f => Eq (Definition n c l t f)
deriving instance DefinitionConstraint Ord n c l t f => Ord (Definition n c l t f)

instance Item (Definition n c l t) where
  prune d =
    case item d of
      DataDefinition i cs ->
        let
          constructor (c, fs) = (Identity $ item c, field <$> fs)
          field (f, t) = (Identity $ item f, prune t)
        in Identity $ DataDefinition (Identity $ item i) $ constructor <$> cs
      _ -> undefined

type ValueBind :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data family ValueBind

data instance ValueBind n c 'LambdaUnlifted t f =
  ValueBindU (f (BindIdentifier n)) (f (Value n c 'LambdaUnlifted t f))
  deriving Generic

data instance ValueBind n c 'LambdaLifted t f
  = ValueBindV (f (BindIdentifier n)) (f (Value n c 'LambdaLifted t f))
  | ValueBindN (f (BindIdentifier n)) [(f (BindIdentifier n), f (Type n c f))] (f (Value n c 'LambdaLifted t f))
  deriving Generic

type ValueBindLambdaUnliftedConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ValueBindLambdaUnliftedConstraint cls n c l t f =
  ( cls (f (BindIdentifier n))
  , cls (f (Value n c l t f))
  )

type ValueBindLambdaLiftedConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ValueBindLambdaLiftedConstraint cls n c l t f =
  ( ValueBindLambdaUnliftedConstraint cls n c l t f
  , cls (f (Type n c f))
  )

deriving instance ValueBindLambdaUnliftedConstraint Show n c 'LambdaUnlifted t f => Show (ValueBind n c 'LambdaUnlifted t f)
deriving instance ValueBindLambdaUnliftedConstraint Read n c 'LambdaUnlifted t f => Read (ValueBind n c 'LambdaUnlifted t f)
deriving instance ValueBindLambdaUnliftedConstraint Eq n c 'LambdaUnlifted t f => Eq (ValueBind n c 'LambdaUnlifted t f)
deriving instance ValueBindLambdaUnliftedConstraint Ord n c 'LambdaUnlifted t f => Ord (ValueBind n c 'LambdaUnlifted t f)

deriving instance ValueBindLambdaLiftedConstraint Show n c 'LambdaLifted t f => Show (ValueBind n c 'LambdaLifted t f)
deriving instance ValueBindLambdaLiftedConstraint Read n c 'LambdaLifted t f => Read (ValueBind n c 'LambdaLifted t f)
deriving instance ValueBindLambdaLiftedConstraint Eq n c 'LambdaLifted t f => Eq (ValueBind n c 'LambdaLifted t f)
deriving instance ValueBindLambdaLiftedConstraint Ord n c 'LambdaLifted t f => Ord (ValueBind n c 'LambdaLifted t f)

newtype CDefinition = CDefinition CExtDecl deriving (Show, Generic)

instance Eq CDefinition where
  (==) = (==) `on` (\(CDefinition c) -> P.render $ C.pretty c)

instance Ord CDefinition where
  compare = compare `on` (\(CDefinition c) -> P.render $ C.pretty c)

data CHeader
  = SystemHeader Text
  | LocalHeader Text
  deriving (Show, Read, Eq, Ord, Generic)

type Type :: NameResolving -> Currying -> (K.Type -> K.Type) -> K.Type
data Type n c f
  = TypeVariable (f (ReferenceIdentifier n))
  | TypeApplication (f (Type n c f)) (f (Type n c f))
  | FunctionType (FunctionType n c f)
  | ProcedureType (f (Type n c f))
  deriving Generic

type TypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> (K.Type -> K.Type) -> K.Constraint
type TypeConstraint cls n c f =
  ( cls (f (ReferenceIdentifier n))
  , cls (FunctionType n c f)
  , cls (f (Type n c f))
  )

deriving instance TypeConstraint Show n c f => Show (Type n c f)
deriving instance TypeConstraint Read n c f => Read (Type n c f)
deriving instance TypeConstraint Eq n c f => Eq (Type n c f)
deriving instance TypeConstraint Ord n c f => Ord (Type n c f)

instance Item (Type n c) where
  prune t =
    case item t of
      TypeVariable i -> Identity $ TypeVariable $ Identity $ item i
      _              -> undefined

type FunctionType :: NameResolving -> Currying -> (K.Type -> K.Type) -> K.Type
data family FunctionType

data instance FunctionType n 'Curried f =
  FunctionTypeC (f (Type n 'Curried f)) (f (Type n 'Curried f))
  deriving Generic

data instance FunctionType n 'Uncurried f =
  FunctionTypeN [f (Type n 'Uncurried f)] (f (Type n 'Uncurried f))
  deriving Generic

type FunctionTypeConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> (K.Type -> K.Type) -> K.Constraint
type FunctionTypeConstraint cls n c f =
  ( cls (f (ReferenceIdentifier n))
  , cls (f (Type n c f))
  )

deriving instance FunctionTypeConstraint Show n 'Curried f => Show (FunctionType n 'Curried f)
deriving instance FunctionTypeConstraint Read n 'Curried f => Read (FunctionType n 'Curried f)
deriving instance FunctionTypeConstraint Eq n 'Curried f => Eq (FunctionType n 'Curried f)
deriving instance FunctionTypeConstraint Ord n 'Curried f => Ord (FunctionType n 'Curried f)

deriving instance FunctionTypeConstraint Show n 'Uncurried f => Show (FunctionType n 'Uncurried f)
deriving instance FunctionTypeConstraint Read n 'Uncurried f => Read (FunctionType n 'Uncurried f)
deriving instance FunctionTypeConstraint Eq n 'Uncurried f => Eq (FunctionType n 'Uncurried f)
deriving instance FunctionTypeConstraint Ord n 'Uncurried f => Ord (FunctionType n 'Uncurried f)

type Value :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data family Value

data instance Value n c l 'Typed f =
  TypedValue (f (Value' n c l 'Typed f)) (f (Type n c f))
  deriving Generic

newtype instance Value n c l 'Untyped f =
  UntypedValue (f (Value' n c l 'Untyped f))
  deriving Generic

type ValueUntypedConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ValueUntypedConstraint cls n c l t f =
  ( cls (f (Value' n c l t f))
  , cls (f (ReferenceIdentifier n))
  , cls (f (BindIdentifier n))
  , cls (f (Literal n c l t f))
  )

type ValueTypedConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ValueTypedConstraint cls n c l t f =
  ( ValueUntypedConstraint cls n c l t f
  , cls (f (Type n c f))
  )

deriving instance ValueUntypedConstraint Show n c l 'Untyped f => Show (Value n c l 'Untyped f)
deriving instance ValueUntypedConstraint Read n c l 'Untyped f => Read (Value n c l 'Untyped f)
deriving instance ValueUntypedConstraint Eq n c l 'Untyped f => Eq (Value n c l 'Untyped f)
deriving instance ValueUntypedConstraint Ord n c l 'Untyped f => Ord (Value n c l 'Untyped f)

deriving instance ValueTypedConstraint Show n c l 'Typed f => Show (Value n c l 'Typed f)
deriving instance ValueTypedConstraint Read n c l 'Typed f => Read (Value n c l 'Typed f)
deriving instance ValueTypedConstraint Eq n c l 'Typed f => Eq (Value n c l 'Typed f)
deriving instance ValueTypedConstraint Ord n c l 'Typed f => Ord (Value n c l 'Typed f)

data Value' n c l t f
  = Variable (ReferenceIdentifier n)
  | Literal (Literal n c l t f)
  | Application (Application n c l t f)
  | Procedure (NonEmpty (f (ProcedureStep n c l t f)))
  | TypeAnnotation (TypeAnnotation n c l t f)
  | Let [f (Definition n c l t f)] (f (Value n c l t f))
  deriving Generic

type Value'Constraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type Value'Constraint cls n c l t f =
  ( cls (ReferenceIdentifier n)
  , cls (f (BindIdentifier n))
  , cls (Literal n c l t f)
  , cls (Application n c l t f)
  , cls (f (ProcedureStep n c l t f))
  , cls (TypeAnnotation n c l t f)
  , cls (f (ValueBind n c l t f))
  , cls (f (Value n c l t f))
  , cls (f (Type n c f))
  , cls (f (Definition n c l t f))
  , cls (f (ProcedureStep n c l t f))
  )

deriving instance Value'Constraint Show n c l t f => Show (Value' n c l t f)
deriving instance Value'Constraint Eq n c l t f => Eq (Value' n c l t f)
deriving instance Value'Constraint Ord n c l t f => Ord (Value' n c l t f)

type TypeAnnotation :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data family TypeAnnotation

data instance TypeAnnotation n c l 'Untyped f =
  TypeAnnotation' (f (Value n c l 'Untyped f)) (f (Type n c f))
  deriving Generic

data instance TypeAnnotation n c l 'Typed f
  deriving Generic

type TypeAnnotationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type TypeAnnotationConstraint cls n c l t f =
  ( cls (f (ReferenceIdentifier n))
  , cls (f (BindIdentifier n))
  , cls (f (Function n c l t f))
  , cls (f (Application n c l t f))
  , cls (f (ProcedureStep n c l t f))
  , cls (f (ValueBind n c l t f))
  , cls (f (FunctionType n c f))
  , cls (f (Value n c l t f))
  , cls (f (Type n c f))
  )

deriving instance TypeAnnotationConstraint Show n c l 'Untyped f => Show (TypeAnnotation n c l 'Untyped f)
deriving instance TypeAnnotationConstraint Eq n c l 'Untyped f => Eq (TypeAnnotation n c l 'Untyped f)
deriving instance TypeAnnotationConstraint Ord n c l 'Untyped f => Ord (TypeAnnotation n c l 'Untyped f)

deriving instance TypeAnnotationConstraint Show n c l 'Typed f => Show (TypeAnnotation n c l 'Typed f)
deriving instance TypeAnnotationConstraint Eq n c l 'Typed f => Eq (TypeAnnotation n c l 'Typed f)
deriving instance TypeAnnotationConstraint Ord n c l 'Typed f => Ord (TypeAnnotation n c l 'Typed f)

data ProcedureStep n c l t f
  = BindProcedure (f (BindIdentifier n)) (f (Value n c l t f))
  | TermProcedure (f (Value n c l t f))

type ProcedureStepConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ProcedureStepConstraint cls n c l t f =
  ( cls (f (BindIdentifier n))
  , cls (f (Value n c l t f))
  )

deriving instance ProcedureStepConstraint Show n c l t f => Show (ProcedureStep n c l t f)
deriving instance ProcedureStepConstraint Eq n c l t f => Eq (ProcedureStep n c l t f)
deriving instance ProcedureStepConstraint Ord n c l t f => Ord (ProcedureStep n c l t f)

data Literal n c l t f
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  | Function (Function n c l t f)
  deriving Generic

type LiteralConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type LiteralConstraint cls n c l t f = cls (Function n c l t f)

deriving instance LiteralConstraint Show n c l t f => Show (Literal n c l t f)
deriving instance LiteralConstraint Eq n c l t f => Eq (Literal n c l t f)
deriving instance LiteralConstraint Ord n c l t f => Ord (Literal n c l t f)

type Application :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data family Application

data instance Application n 'Curried l t f =
  ApplicationC (f (Value n 'Curried l t f)) (f (Value n 'Curried l t f))
  deriving Generic

data instance Application n 'Uncurried l t f =
  ApplicationN (f (Value n 'Uncurried l t f)) [f (Value n 'Uncurried l t f)]
  deriving Generic

type ApplicationConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type ApplicationConstraint cls n c l t f = (cls (f (Value n c l t f)))

deriving instance ApplicationConstraint Show n 'Curried l t f => Show (Application n 'Curried l t f)
deriving instance ApplicationConstraint Eq n 'Curried l t f => Eq (Application n 'Curried l t f)
deriving instance ApplicationConstraint Ord n 'Curried l t f => Ord (Application n 'Curried l t f)

deriving instance ApplicationConstraint Show n 'Uncurried l t f => Show (Application n 'Uncurried l t f)
deriving instance ApplicationConstraint Eq n 'Uncurried l t f => Eq (Application n 'Uncurried l t f)
deriving instance ApplicationConstraint Ord n 'Uncurried l t f => Ord (Application n 'Uncurried l t f)

type Function :: NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Type
data family Function

data instance Function n 'Curried 'LambdaUnlifted t f =
  FunctionC (f (BindIdentifier n)) (f (Type n 'Curried f)) (f (Value n 'Curried 'LambdaUnlifted t f))
  deriving Generic

data instance Function n 'Uncurried 'LambdaUnlifted t f =
  FunctionN [(f (BindIdentifier n), f (Type n 'Uncurried f))] (f (Value n 'Uncurried 'LambdaUnlifted t f))
  deriving Generic

data instance Function n c 'LambdaLifted t f deriving (Show, Read, Eq, Ord, Generic)

type FunctionConstraint :: (K.Type -> K.Constraint) -> NameResolving -> Currying -> LambdaLifting -> Typing -> (K.Type -> K.Type) -> K.Constraint
type FunctionConstraint cls n c l t f =
  ( cls (f (BindIdentifier n))
  , cls (f (ReferenceIdentifier n))
  , cls (f (Value n c l t f))
  , cls (f (Type n c f))
  )

deriving instance FunctionConstraint Show n 'Curried 'LambdaUnlifted t f => Show (Function n 'Curried 'LambdaUnlifted t f)
deriving instance FunctionConstraint Eq n 'Curried 'LambdaUnlifted t f => Eq (Function n 'Curried 'LambdaUnlifted t f)
deriving instance FunctionConstraint Ord n 'Curried 'LambdaUnlifted t f => Ord (Function n 'Curried 'LambdaUnlifted t f)

deriving instance FunctionConstraint Show n 'Uncurried 'LambdaUnlifted t f => Show (Function n 'Uncurried 'LambdaUnlifted t f)
deriving instance FunctionConstraint Eq n 'Uncurried 'LambdaUnlifted t f => Eq (Function n 'Uncurried 'LambdaUnlifted t f)
deriving instance FunctionConstraint Ord n 'Uncurried 'LambdaUnlifted t f => Ord (Function n 'Uncurried 'LambdaUnlifted t f)

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

instance Pretty ModuleName where
  pretty (ModuleName ts) = fold $ N.intersperse "." ts

class Item a where
  prune :: Metadata f => f (a f) -> Identity (a Identity)

class Metadata f where
  item :: f a -> a

class (Traversable f, Metadata f) => HasPosition f where
  range :: f a -> Maybe (Position, Position)

data Position
  = Position { line :: Word, column :: Word }
  deriving (Show, Read, Eq, Ord, Generic)

data AttachPosition a =
  AttachPosition Position Position a
  deriving (Show, Read, Eq, Ord, Generic)

instance Functor AttachPosition where
  fmap f (AttachPosition p1 p2 a) = AttachPosition p1 p2 $ f a

instance Foldable AttachPosition where
  foldMap f (AttachPosition _ _ a) = f a

instance Traversable AttachPosition where
  traverse f (AttachPosition p1 p2 a) = AttachPosition p1 p2 <$> f a

instance Metadata AttachPosition where
  item (AttachPosition _ _ a) = a

instance HasPosition AttachPosition where
  range (AttachPosition begin end _) = Just (begin, end)

instance Metadata Identity where
  item (Identity a) = a

instance HasPosition Identity where
  range (Identity _) = Nothing

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

class Pretty a where
  pretty :: a -> Text
