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
import           Language.Kmkm.Syntax.Base  (Currying (Curried), LambdaLifting (LambdaUnlifted), Typing (Typed))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Curried 'LambdaUnlifted 'Typed

type Member = S.Member 'Curried 'LambdaUnlifted 'Typed

type Bind = S.Bind 'Curried 'LambdaUnlifted 'Typed

type Type = T.Type 'Curried

type Term = V.Term 'Curried 'LambdaUnlifted 'Typed

type Term' = V.Term' 'Curried 'LambdaUnlifted 'Typed

type Literal = V.Literal 'Curried 'LambdaUnlifted 'Typed

type Function = V.Function 'Curried 'LambdaUnlifted 'Typed

type Application = V.Application 'Curried 'LambdaUnlifted 'Typed

type Arrow = T.Arrow 'Curried
