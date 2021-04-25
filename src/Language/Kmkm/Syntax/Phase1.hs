{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase1
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
import           Language.Kmkm.Syntax.Base  (Curriness (Curried))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Curried

type Member = S.Member 'Curried

type Bind = S.Bind 'Curried

type Type = T.Type 'Curried

type Term = V.Term 'Curried

type Literal = V.Literal 'Curried

type Function = V.Function 'Curried

type Application = V.Application 'Curried

type Arrow = T.Arrow 'Curried
