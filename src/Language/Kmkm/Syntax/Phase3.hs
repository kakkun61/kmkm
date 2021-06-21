{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase3
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

import           Language.Kmkm.Syntax (Currying (Uncurried), LambdaLifting (LambdaUnlifted), Typing (Typed))
import qualified Language.Kmkm.Syntax as S

type Module = S.Module 'Uncurried 'LambdaUnlifted 'Typed

type Definition = S.Definition 'Uncurried 'LambdaUnlifted 'Typed

type ValueBind = S.ValueBind 'Uncurried 'LambdaUnlifted 'Typed

type Type = S.Type 'Uncurried

type Term = S.Term 'Uncurried 'LambdaUnlifted 'Typed

type Term' = S.Term' 'Uncurried 'LambdaUnlifted 'Typed

type ProcedureStep = S.ProcedureStep 'Uncurried 'LambdaUnlifted 'Typed

type Literal = S.Literal 'Uncurried 'LambdaUnlifted 'Typed

type Function = S.Function 'Uncurried 'LambdaUnlifted 'Typed

type Application = S.Application 'Uncurried 'LambdaUnlifted 'Typed

type FunctionType = S.FunctionType 'Uncurried
