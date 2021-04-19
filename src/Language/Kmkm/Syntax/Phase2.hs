{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Syntax.Phase2
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

type Module = S.Module Function Application T.ArrowN Term

type Member = S.Member Function Application T.ArrowN Term

type Bind = S.Bind Function Application T.ArrowN Term

type Type = T.Type T.ArrowN

newtype Term = Term Term' deriving (Show, Read, Eq, Ord, Generic)

type Term' = V.Term Function Application Term

type Literal = V.Literal Function

newtype Function = Function Function' deriving (Show, Read, Eq, Ord, Generic)

type Function' = V.FunctionN Application Term

newtype Application = Application Application' deriving (Show, Read, Eq, Ord, Generic)

type Application' = V.ApplicationN Function Term
