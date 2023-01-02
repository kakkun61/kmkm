{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Language.Kmkm.Internal.Syntax.Core.NameUnresolved.Curried
  ( -- * Types
    Type (..)
  ) where

import Language.Kmkm.Internal.Syntax.Core.Common         (Pretty (pretty), Pretty1)
import Language.Kmkm.Internal.Syntax.Core.NameUnresolved (BindIdentifier, ReferenceIdentifier)

import           Data.Copointed              (Copointed (copoint))
import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Classes        (Eq1, Ord1, Read1, Show1)
import           Data.Functor.F              (F)
import qualified Data.Functor.F              as F
import qualified Data.Kind                   as K
import           Data.Pointed                (Pointed (point))
import           Data.Text                   (Text)
import           GHC.Exts                    (IsList, IsString (fromString))
import qualified GHC.Exts                    as E
import           GHC.Generics                (Generic)

-- Type

type Type :: (K.Type -> K.Type) -> K.Type
data Type f
  = TypeVariable (F f ReferenceIdentifier)
  | TypeApplication (F f (Type f)) (F f (Type f))
  | FunctionType (F f (Type f)) (F f (Type f))
  | ProcedureType (F f (Type f))
  | ForAllType (F f BindIdentifier) (F f (Type f))
  deriving Generic

deriving instance Show1 f => Show (Type f)
deriving instance Read1 f => Read (Type f)
deriving instance Eq1 f => Eq (Type f)
deriving instance Ord1 f => Ord (Type f)

instance FunctorB Type where
  bmap f (TypeVariable i)        = TypeVariable (F.map f i)
  bmap f (TypeApplication t1 t2) = TypeApplication (bmap f <$> F.map f t1) (bmap f <$> F.map f t2)
  bmap f (FunctionType t1 t2)    = FunctionType (bmap f <$> F.map f t1) (bmap f <$> F.map f t2)
  bmap f (ProcedureType t)       = ProcedureType (bmap f <$> F.map f t)
  bmap f (ForAllType i t)        = ForAllType (F.map f i) (bmap f <$> F.map f t)

instance (IsString ReferenceIdentifier, Pointed f) => IsString (Type f) where
  fromString = TypeVariable . point . fromString

instance (IsList ReferenceIdentifier, E.Item ReferenceIdentifier ~ Text, Pointed f, Copointed f) => IsList (Type f) where
  type Item (Type f) = Text
  fromList = TypeVariable . point . E.fromList
  toList (TypeVariable v) = E.toList $ copoint v
  toList _                = error "only type variable acceptable"

instance Pretty1 f => Pretty (Type f) where
  pretty (TypeVariable i)        = pretty i
  pretty (TypeApplication t1 t2) = "(apply " <> pretty t1 <> " " <> pretty t2 <> ")"
  pretty (FunctionType t1 t2)    = "(function " <> pretty t1 <> " " <> pretty t2 <> ")"
  pretty (ProcedureType t)       = "(procedure " <> pretty t <> ")"
  pretty (ForAllType i t)        = "(for-all " <> pretty i <> " " <> pretty t <> ")"
