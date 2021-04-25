{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase2
  ( Module
  , Member
  , Bind
  , Type
  , Term
  , Literal
  , Function
  , Application
  , Arrow
  ) where

import qualified Language.Kmkm.Syntax       as S
import           Language.Kmkm.Syntax.Base  (Curriness (Uncurried))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Uncurried

type Member = S.Member 'Uncurried

type Bind = S.Bind 'Uncurried

type Type = T.Type 'Uncurried

type Term = V.Term 'Uncurried

type Literal = V.Literal 'Uncurried

type Function = V.Function 'Uncurried

type Application = V.Application 'Uncurried

type Arrow = T.Arrow 'Uncurried
