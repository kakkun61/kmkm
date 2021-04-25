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

data Module c =
  Module Identifier [Member c]
  deriving Generic

deriving instance Show (Member c) => Show (Module c)
deriving instance Read (Member c) => Read (Module c)
deriving instance Eq (Member c) => Eq (Module c)
deriving instance Ord (Member c) => Ord (Module c)

data Member c
  = Definition Identifier [(Identifier, [(Identifier, Type c)])]
  | Bind (Bind c)
  deriving Generic

deriving instance (Show (Type c), Show (Application c), Show (Function c)) => Show (Member c)
deriving instance (Read (Type c), Read (Application c), Read (Function c), Show (Application c), Show (Function c)) => Read (Member c)
deriving instance (Eq (Type c), Eq (Application c), Eq (Function c)) => Eq (Member c)
deriving instance (Ord (Type c), Ord (Application c), Ord (Function c)) => Ord (Member c)

data Bind c
  = Type Identifier (Type c)
  | Term Identifier (Term c) (Type c)
  deriving Generic

deriving instance (Show (Term c), Show (Type c)) => Show (Bind c)
deriving instance (Read (Type c), Read (Application c), Read (Function c), Show (Application c), Show (Function c)) => Read (Bind c)
deriving instance (Eq (Type c), Eq (Application c), Eq (Function c)) => Eq (Bind c)
deriving instance (Ord (Type c), Ord (Application c), Ord (Function c)) => Ord (Bind c)
