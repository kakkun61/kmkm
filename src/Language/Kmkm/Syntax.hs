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
  , ValueBind (..)
  , C (..)
  ) where

import Language.Kmkm.Syntax.Base  (Currying, Identifier, LambdaLifting (LambdaLifted, LambdaUnlifted), ModuleName,
                                   Typing)
import Language.Kmkm.Syntax.Type  (Type)
import Language.Kmkm.Syntax.Value (Term)

import           Data.Function         (on)
import qualified Data.Kind             as K
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import qualified Language.C.Pretty     as C
import           Language.C.Syntax.AST (CExtDecl)
import qualified Text.PrettyPrint      as P

data Module c l t =
  Module ModuleName [Member c l t]
  deriving Generic

deriving instance Show (Member c l t) => Show (Module c l t)
deriving instance Read (Member c l t) => Read (Module c l t)
deriving instance Eq (Member c l t) => Eq (Module c l t)
deriving instance Ord (Member c l t) => Ord (Module c l t)

data Member c l t
  = Definition Identifier [(Identifier, [(Identifier, Type c)])]
  | TypeBind Identifier (Type c)
  | ValueBind (ValueBind c l t) [Member c l t]
  | ForeignValueBind Identifier [Text] C (Type c)
  deriving Generic

deriving instance (Show (Term c l t), Show (Type c), Show (ValueBind c l t)) => Show (Member c l t)
deriving instance (Eq (Term c l t), Eq (Type c), Eq (ValueBind c l t)) => Eq (Member c l t)
deriving instance (Ord (Term c l t), Ord (Type c), Ord (ValueBind c l t)) => Ord (Member c l t)

type ValueBind :: Currying -> LambdaLifting -> Typing -> K.Type
data family ValueBind

data instance ValueBind c 'LambdaUnlifted t =
  ValueBindU Identifier (Term c 'LambdaUnlifted t)
  deriving Generic

data instance ValueBind c 'LambdaLifted t
  = ValueBindV Identifier (Term c 'LambdaLifted t)
  | ValueBindN Identifier [(Identifier, Type c)] (Term c 'LambdaLifted t)

deriving instance (Show (Term c 'LambdaUnlifted t), Show (Type c)) => Show (ValueBind c 'LambdaUnlifted t)
deriving instance (Read (Term c 'LambdaUnlifted t), Read (Type c)) => Read (ValueBind c 'LambdaUnlifted t)
deriving instance (Eq (Term c 'LambdaUnlifted t), Eq (Type c)) => Eq (ValueBind c 'LambdaUnlifted t)
deriving instance (Ord (Term c 'LambdaUnlifted t), Ord (Type c)) => Ord (ValueBind c 'LambdaUnlifted t)

deriving instance (Show (Term c 'LambdaLifted t), Show (Type c)) => Show (ValueBind c 'LambdaLifted t)
deriving instance (Read (Term c 'LambdaLifted t), Read (Type c)) => Read (ValueBind c 'LambdaLifted t)
deriving instance (Eq (Term c 'LambdaLifted t), Eq (Type c)) => Eq (ValueBind c 'LambdaLifted t)
deriving instance (Ord (Term c 'LambdaLifted t), Ord (Type c)) => Ord (ValueBind c 'LambdaLifted t)

newtype C = C CExtDecl deriving (Show, Generic)

instance Eq C where
  (==) = (==) `on` (\(C c) -> P.render $ C.pretty c)

instance Ord C where
  compare = compare `on` (\(C c) -> P.render $ C.pretty c)
