{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase5
  ( Module
  , Member
  , ValueBind
  , Type
  , Term
  , Term'
  , ProcedureStep
  , Literal
  , Function
  , Application
  , TFunction
  ) where

import qualified Language.Kmkm.Syntax       as S
import           Language.Kmkm.Syntax.Base  (Currying (Uncurried), LambdaLifting (LambdaLifted), Typing (Typed))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Uncurried 'LambdaLifted 'Typed

type Member = S.Member 'Uncurried 'LambdaLifted 'Typed

type ValueBind = S.ValueBind 'Uncurried 'LambdaLifted 'Typed

type Type = T.Type 'Uncurried

type Term = V.Term 'Uncurried 'LambdaLifted 'Typed

type Term' = V.Term' 'Uncurried 'LambdaLifted 'Typed

type ProcedureStep = V.ProcedureStep 'Uncurried 'LambdaLifted 'Typed

type Literal = V.Literal 'Uncurried 'LambdaLifted 'Typed

type Function = V.Function 'Uncurried 'LambdaLifted 'Typed

type Application = V.Application 'Uncurried 'LambdaLifted 'Typed

type TFunction = T.Function 'Uncurried
