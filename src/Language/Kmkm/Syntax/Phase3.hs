{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase3
  ( Module
  , Member
  , Bind
  , TermBind
  , Type
  , Term
  , Term'
  , ProcedureStep
  , Literal
  , Function
  , Application
  , Arrow
  ) where

import qualified Language.Kmkm.Syntax       as S
import           Language.Kmkm.Syntax.Base  (Currying (Uncurried), LambdaLifting (LambdaUnlifted), Typing (Typed))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Uncurried 'LambdaUnlifted 'Typed

type Member = S.Member 'Uncurried 'LambdaUnlifted 'Typed

type Bind = S.Bind 'Uncurried 'LambdaUnlifted 'Typed

type TermBind = S.TermBind 'Uncurried 'LambdaUnlifted 'Typed

type Type = T.Type 'Uncurried

type Term = V.Term 'Uncurried 'LambdaUnlifted 'Typed

type Term' = V.Term' 'Uncurried 'LambdaUnlifted 'Typed

type ProcedureStep = V.ProcedureStep 'Uncurried 'LambdaUnlifted 'Typed

type Literal = V.Literal 'Uncurried 'LambdaUnlifted 'Typed

type Function = V.Function 'Uncurried 'LambdaUnlifted 'Typed

type Application = V.Application 'Uncurried 'LambdaUnlifted 'Typed

type Arrow = T.Arrow 'Uncurried
