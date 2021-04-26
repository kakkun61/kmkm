{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase1
  ( Module
  , Member
  , Bind
  , Type
  , Term
  , Term'
  , Literal
  , Function
  , Application
  , Arrow
  ) where

import qualified Language.Kmkm.Syntax       as S
import           Language.Kmkm.Syntax.Base  (Curriness (Curried), Typing (Untyped))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Curried 'Untyped

type Member = S.Member 'Curried 'Untyped

type Bind = S.Bind 'Curried 'Untyped

type Type = T.Type 'Curried

type Term = V.Term 'Curried 'Untyped

type Term' = V.Term' 'Curried 'Untyped

type Literal = V.Literal 'Curried 'Untyped

type Function = V.Function 'Curried 'Untyped

type Application = V.Application 'Curried 'Untyped

type Arrow = T.Arrow 'Curried
