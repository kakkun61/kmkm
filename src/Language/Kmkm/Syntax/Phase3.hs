{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase3
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
import           Language.Kmkm.Syntax.Base  (Curriness (Uncurried), Typing (Typed))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Uncurried 'Typed

type Member = S.Member 'Uncurried 'Typed

type Bind = S.Bind 'Uncurried 'Typed

type Type = T.Type 'Uncurried

type Term = V.Term 'Uncurried 'Typed

type Term' = V.Term' 'Uncurried 'Typed

type Literal = V.Literal 'Uncurried 'Typed

type Function = V.Function 'Uncurried 'Typed

type Application = V.Application 'Uncurried 'Typed

type Arrow = T.Arrow 'Uncurried
