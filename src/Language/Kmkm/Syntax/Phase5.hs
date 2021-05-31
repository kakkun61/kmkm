{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Syntax.Phase5
  ( Module
  , Member
  , Bind
  , TermBind
  , Type
  , Term
  , Term'
  , Procedure
  , Literal
  , Function
  , Application
  , Arrow
  ) where

import qualified Language.Kmkm.Syntax       as S
import           Language.Kmkm.Syntax.Base  (Currying (Uncurried), LambdaLifting (LambdaLifted), Typing (Typed))
import qualified Language.Kmkm.Syntax.Type  as T
import qualified Language.Kmkm.Syntax.Value as V

type Module = S.Module 'Uncurried 'LambdaLifted 'Typed

type Member = S.Member 'Uncurried 'LambdaLifted 'Typed

type Bind = S.Bind 'Uncurried 'LambdaLifted 'Typed

type TermBind = S.TermBind 'Uncurried 'LambdaLifted 'Typed

type Type = T.Type 'Uncurried

type Term = V.Term 'Uncurried 'LambdaLifted 'Typed

type Term' = V.Term' 'Uncurried 'LambdaLifted 'Typed

type Procedure = V.Procedure 'Uncurried 'LambdaLifted 'Typed

type Literal = V.Literal 'Uncurried 'LambdaLifted 'Typed

type Function = V.Function 'Uncurried 'LambdaLifted 'Typed

type Application = V.Application 'Uncurried 'LambdaLifted 'Typed

type Arrow = T.Arrow 'Uncurried