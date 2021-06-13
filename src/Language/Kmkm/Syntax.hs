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
                                   Typing)
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
  | TermBind (TermBind c l t) [Member c l t]
  deriving Generic

deriving instance (Show (Term c l t), Show (Type c), Show (TermBind c l t)) => Show (Bind c l t)
deriving instance (Read (Term c l t), Read (Type c), Read (TermBind c l t)) => Read (Bind c l t)
deriving instance (Eq (Term c l t), Eq (Type c), Eq (TermBind c l t)) => Eq (Bind c l t)
deriving instance (Ord (Term c l t), Ord (Type c), Ord (TermBind c l t)) => Ord (Bind c l t)

type TermBind :: Currying -> LambdaLifting -> Typing -> K.Type
data family TermBind

data instance TermBind c 'LambdaUnlifted t =
  TermBindU Identifier (Term c 'LambdaUnlifted t)
  deriving Generic

data instance TermBind c 'LambdaLifted t
  = TermBindV Identifier (Term c 'LambdaLifted t)
  | TermBindN Identifier [(Identifier, Type c)] (Term c 'LambdaLifted t)

deriving instance (Show (Term c 'LambdaUnlifted t), Show (Type c)) => Show (TermBind c 'LambdaUnlifted t)
deriving instance (Read (Term c 'LambdaUnlifted t), Read (Type c)) => Read (TermBind c 'LambdaUnlifted t)
deriving instance (Eq (Term c 'LambdaUnlifted t), Eq (Type c)) => Eq (TermBind c 'LambdaUnlifted t)
deriving instance (Ord (Term c 'LambdaUnlifted t), Ord (Type c)) => Ord (TermBind c 'LambdaUnlifted t)

deriving instance (Show (Term c 'LambdaLifted t), Show (Type c)) => Show (TermBind c 'LambdaLifted t)
deriving instance (Read (Term c 'LambdaLifted t), Read (Type c)) => Read (TermBind c 'LambdaLifted t)
deriving instance (Eq (Term c 'LambdaLifted t), Eq (Type c)) => Eq (TermBind c 'LambdaLifted t)
deriving instance (Ord (Term c 'LambdaLifted t), Ord (Type c)) => Ord (TermBind c 'LambdaLifted t)
