{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase2
  ( Module
  , Definition
  , Type
  , Term
  , Term'
  , ProcedureStep
  , Literal
  , Function
  , Application
  , FunctionType
  ) where

import           Language.Kmkm.Syntax (Currying (Curried), LambdaLifting (LambdaUnlifted), Typing (Typed))
import qualified Language.Kmkm.Syntax as S

type Module = S.Module 'Curried 'LambdaUnlifted 'Typed

type Definition = S.Definition 'Curried 'LambdaUnlifted 'Typed

type Type = S.Type 'Curried

type Term = S.Term 'Curried 'LambdaUnlifted 'Typed

type Term' = S.Term' 'Curried 'LambdaUnlifted 'Typed

type ProcedureStep = S.ProcedureStep 'Curried 'LambdaUnlifted 'Typed

type Literal = S.Literal 'Curried 'LambdaUnlifted 'Typed

type Function = S.Function 'Curried 'LambdaUnlifted 'Typed

type Application = S.Application 'Curried 'LambdaUnlifted 'Typed

type FunctionType = S.FunctionType 'Curried
