{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase1
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
  , TypeAnnotation
  , FunctionType
  ) where

import           Language.Kmkm.Syntax (Currying (Curried), LambdaLifting (LambdaUnlifted), Typing (Untyped))
import qualified Language.Kmkm.Syntax as S

type Module = S.Module 'Curried 'LambdaUnlifted 'Untyped

type Definition = S.Definition 'Curried 'LambdaUnlifted 'Untyped

type ValueBind = S.ValueBind 'Curried 'LambdaUnlifted 'Untyped

type Type = S.Type 'Curried

type Term = S.Term 'Curried 'LambdaUnlifted 'Untyped

type Term' = S.Term' 'Curried 'LambdaUnlifted 'Untyped

type ProcedureStep = S.ProcedureStep 'Curried 'LambdaUnlifted 'Untyped

type Literal = S.Literal 'Curried 'LambdaUnlifted 'Untyped

type Function = S.Function 'Curried 'LambdaUnlifted 'Untyped

type Application = S.Application 'Curried 'LambdaUnlifted 'Untyped

type TypeAnnotation = S.TypeAnnotation 'Curried 'LambdaUnlifted 'Untyped

type FunctionType = S.FunctionType 'Curried
