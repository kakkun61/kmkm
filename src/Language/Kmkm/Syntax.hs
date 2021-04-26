{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Kmkm.Syntax
  ( Module (..)
  , Member (..)
  , Bind (..)
  ) where

import Language.Kmkm.Syntax.Base  (Identifier)
import Language.Kmkm.Syntax.Type  (Type)
import Language.Kmkm.Syntax.Value (Application, Function, Term)

import GHC.Generics (Generic)

data Module c t =
  Module Identifier [Member c t]
  deriving Generic

deriving instance Show (Member c t) => Show (Module c t)
deriving instance Read (Member c t) => Read (Module c t)
deriving instance Eq (Member c t) => Eq (Module c t)
deriving instance Ord (Member c t) => Ord (Module c t)

data Member c t
  = Definition Identifier [(Identifier, [(Identifier, Type c)])]
  | Bind (Bind c t)
  deriving Generic

deriving instance (Show (Term c t), Show (Type c), Show (Application c t), Show (Function c t)) => Show (Member c t)
deriving instance (Read (Term c t), Read (Type c), Read (Application c t), Read (Function c t), Show (Application c t), Show (Function c t)) => Read (Member c t)
deriving instance (Ord (Term c t), Eq (Type c), Eq (Application c t), Eq (Function c t)) => Eq (Member c t)
deriving instance (Ord (Term c t), Ord (Type c), Ord (Application c t), Ord (Function c t)) => Ord (Member c t)

data Bind c t
  = Type Identifier (Type c)
  | Term Identifier (Term c t) (Type c)
  deriving Generic

deriving instance (Show (Term c t), Show (Type c)) => Show (Bind c t)
deriving instance (Read (Term c t), Read (Type c), Read (Application c t), Read (Function c t), Show (Application c t), Show (Function c t)) => Read (Bind c t)
deriving instance (Eq (Term c t), Eq (Type c), Eq (Application c t), Eq (Function c t)) => Eq (Bind c t)
deriving instance (Ord (Term c t), Ord (Type c), Ord (Application c t), Ord (Function c t)) => Ord (Bind c t)
