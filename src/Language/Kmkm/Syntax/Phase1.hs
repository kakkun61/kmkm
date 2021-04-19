{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Phase1
  ( Module
  , Member
  , Bind
  , Type
  , Term (..)
  , Term'
  , Literal
  , Function (..)
  , Function'
  , Application (..)
  , Application'
  ) where

import           GHC.Generics               (Generic)
import qualified Language.Kmkm.Syntax       as S
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module Function Application T.Arrow Term

type Member = S.Member Function Application T.Arrow Term

type Bind = S.Bind Function Application T.Arrow Term

type Type = T.Type T.Arrow

newtype Term = Term Term' deriving (Show, Read, Eq, Ord, Generic)

type Term' = V.Term Function Application Term

type Literal = V.Literal Function

newtype Function = Function Function' deriving (Show, Read, Eq, Ord, Generic)

type Function' = V.Function Application Term

newtype Application = Application Application' deriving (Show, Read, Eq, Ord, Generic)

type Application' = V.Application Function Term
