{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase1
  ( Module
  , Member
  , Bind
  , ValueBind
  , Type
  , Term
  , Term'
  , ProcedureStep
  , Literal
  , Function
  , Application
  , TypeAnnotation
  , TFunction
  ) where

import qualified Language.Kmkm.Syntax       as S
import           Language.Kmkm.Syntax.Base  (Currying (Curried), LambdaLifting (LambdaUnlifted), Typing (Untyped))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Curried 'LambdaUnlifted 'Untyped

type Member = S.Member 'Curried 'LambdaUnlifted 'Untyped

type Bind = S.Bind 'Curried 'LambdaUnlifted 'Untyped

type ValueBind = S.ValueBind 'Curried 'LambdaUnlifted 'Untyped

type Type = T.Type 'Curried

type Term = V.Term 'Curried 'LambdaUnlifted 'Untyped

type Term' = V.Term' 'Curried 'LambdaUnlifted 'Untyped

type ProcedureStep = V.ProcedureStep 'Curried 'LambdaUnlifted 'Untyped

type Literal = V.Literal 'Curried 'LambdaUnlifted 'Untyped

type Function = V.Function 'Curried 'LambdaUnlifted 'Untyped

type Application = V.Application 'Curried 'LambdaUnlifted 'Untyped

type TypeAnnotation = V.TypeAnnotation 'Curried 'LambdaUnlifted 'Untyped

type TFunction = T.Function 'Curried
