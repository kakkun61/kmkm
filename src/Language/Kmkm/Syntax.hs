{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Language.Kmkm.Syntax
  ( Module (..)
  , Member (..)
  , Bind (..)
  , TermBind (..)
  ) where

import Language.Kmkm.Syntax.Base  (Currying, Identifier, LambdaLifting (LambdaLifted, LambdaUnlifted), ModuleName,
                                   Typing (Typed, Untyped))
import Language.Kmkm.Syntax.Type  (Type)
import Language.Kmkm.Syntax.Value (Term)

import qualified Data.Kind    as K
import           GHC.Generics (Generic)

data Module c l t =
  Module ModuleName [Member c l t]
  deriving Generic

deriving instance Show (Member c l t) => Show (Module c l t)
deriving instance Read (Member c l t) => Read (Module c l t)
deriving instance Eq (Member c l t) => Eq (Module c l t)
deriving instance Ord (Member c l t) => Ord (Module c l t)

data Member c l t
  = Definition Identifier [(Identifier, [(Identifier, Type c)])]
  | Bind (Bind c l t)
  deriving Generic

deriving instance (Show (Type c), Show (Bind c l t)) => Show (Member c l t)
deriving instance (Read (Type c), Read (Bind c l t)) => Read (Member c l t)
deriving instance (Eq (Type c), Eq (Bind c l t)) => Eq (Member c l t)
deriving instance (Ord (Type c), Ord (Bind c l t)) => Ord (Member c l t)

data Bind c l t
  = TypeBind Identifier (Type c)
  | TermBind (TermBind c l t)
  deriving Generic

deriving instance (Show (Term c l t), Show (Type c), Show (TermBind c l t)) => Show (Bind c l t)
deriving instance (Read (Term c l t), Read (Type c), Read (TermBind c l t)) => Read (Bind c l t)
deriving instance (Eq (Term c l t), Eq (Type c), Eq (TermBind c l t)) => Eq (Bind c l t)
deriving instance (Ord (Term c l t), Ord (Type c), Ord (TermBind c l t)) => Ord (Bind c l t)

type TermBind :: Currying -> LambdaLifting -> Typing -> K.Type
data family TermBind

data instance TermBind c 'LambdaUnlifted 'Typed =
  TermBindUT Identifier (Term c 'LambdaUnlifted 'Typed)
  deriving Generic

data instance TermBind c 'LambdaUnlifted 'Untyped =
  TermBindUU Identifier (Term c 'LambdaUnlifted 'Untyped) (Type c)
  deriving Generic

data instance TermBind c 'LambdaLifted 'Typed
  = TermBindV Identifier (Term c 'LambdaLifted 'Typed)
  | TermBind0 Identifier (Term c 'LambdaLifted 'Typed)
  | TermBind1 Identifier Identifier (Type c) (Term c 'LambdaLifted 'Typed)
  | TermBind2 Identifier Identifier (Type c) Identifier (Type c) (Term c 'LambdaLifted 'Typed)
  | TermBind3 Identifier Identifier (Type c) Identifier (Type c) Identifier (Type c) (Term c 'LambdaLifted 'Typed)

deriving instance (Show (Term c 'LambdaUnlifted 'Typed), Show (Type c)) => Show (TermBind c 'LambdaUnlifted 'Typed)
deriving instance (Read (Term c 'LambdaUnlifted 'Typed), Read (Type c)) => Read (TermBind c 'LambdaUnlifted 'Typed)
deriving instance (Eq (Term c 'LambdaUnlifted 'Typed), Eq (Type c)) => Eq (TermBind c 'LambdaUnlifted 'Typed)
deriving instance (Ord (Term c 'LambdaUnlifted 'Typed), Ord (Type c)) => Ord (TermBind c 'LambdaUnlifted 'Typed)

deriving instance (Show (Term c 'LambdaUnlifted 'Untyped), Show (Type c)) => Show (TermBind c 'LambdaUnlifted 'Untyped)
deriving instance (Read (Term c 'LambdaUnlifted 'Untyped), Read (Type c)) => Read (TermBind c 'LambdaUnlifted 'Untyped)
deriving instance (Eq (Term c 'LambdaUnlifted 'Untyped), Eq (Type c)) => Eq (TermBind c 'LambdaUnlifted 'Untyped)
deriving instance (Ord (Term c 'LambdaUnlifted 'Untyped), Ord (Type c)) => Ord (TermBind c 'LambdaUnlifted 'Untyped)

deriving instance (Show (Term c 'LambdaLifted 'Typed), Show (Type c)) => Show (TermBind c 'LambdaLifted 'Typed)
deriving instance (Read (Term c 'LambdaLifted 'Typed), Read (Type c)) => Read (TermBind c 'LambdaLifted 'Typed)
deriving instance (Eq (Term c 'LambdaLifted 'Typed), Eq (Type c)) => Eq (TermBind c 'LambdaLifted 'Typed)
deriving instance (Ord (Term c 'LambdaLifted 'Typed), Ord (Type c)) => Ord (TermBind c 'LambdaLifted 'Typed)
