{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase5
  ( Module
  , Definition
  , ValueBind
  , Type
  , Term
  , Term'
  , ProcedureStep
  , Literal
  , Function
  , Application
  , FunctionType
  ) where

import           Language.Kmkm.Syntax (Currying (Uncurried), LambdaLifting (LambdaLifted), Typing (Typed))
import qualified Language.Kmkm.Syntax as S

type Module = S.Module 'Uncurried 'LambdaLifted 'Typed

type Definition = S.Definition 'Uncurried 'LambdaLifted 'Typed

type ValueBind = S.ValueBind 'Uncurried 'LambdaLifted 'Typed

type Type = S.Type 'Uncurried

type Term = S.Term 'Uncurried 'LambdaLifted 'Typed

type Term' = S.Term' 'Uncurried 'LambdaLifted 'Typed

type ProcedureStep = S.ProcedureStep 'Uncurried 'LambdaLifted 'Typed

type Literal = S.Literal 'Uncurried 'LambdaLifted 'Typed

type Function = S.Function 'Uncurried 'LambdaLifted 'Typed

type Application = S.Application 'Uncurried 'LambdaLifted 'Typed

type FunctionType = S.FunctionType 'Uncurried
