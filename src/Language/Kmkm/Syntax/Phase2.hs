{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase2
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
import           Language.Kmkm.Syntax.Base  (Curriness (Curried), Typing (Typed))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Curried 'Typed

type Member = S.Member 'Curried 'Typed

type Bind = S.Bind 'Curried 'Typed

type Type = T.Type 'Curried

type Term = V.Term 'Curried 'Typed

type Term' = V.Term' 'Curried 'Typed

type Literal = V.Literal 'Curried 'Typed

type Function = V.Function 'Curried 'Typed

type Application = V.Application 'Curried 'Typed

type Arrow = T.Arrow 'Curried
