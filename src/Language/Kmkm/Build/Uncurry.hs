{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Build.Uncurry
  ( uncurry
  , Module
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Prelude            (($), (<$>))

type Module c = S.Module 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type Definition c = S.Definition 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type Value c = S.Value 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type ProcedureStep c = S.ProcedureStep 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type Literal c = S.Literal 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type Application c = S.Application 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type Function c = S.Function 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed

type Type c = S.Type 'S.NameResolved c

type FunctionType c = S.FunctionType 'S.NameResolved c

uncurry :: Module 'S.Curried -> Module 'S.Uncurried
uncurry = module'

module' :: Module 'S.Curried -> Module 'S.Uncurried
module' (S.Module i ds ms) = S.Module i ds $ definition <$> ms

definition :: Definition 'S.Curried -> Definition 'S.Uncurried
definition (S.DataDefinition i cs) =
  S.DataDefinition i $ go <$> cs
  where
    go (i, fs) =
      (i, go <$> fs)
      where
        go (i, t) = (i, typ t)
definition (S.TypeBind i t) = S.TypeBind i $ typ t
definition (S.ValueBind (S.ValueBindU i v)) = S.ValueBind (S.ValueBindU i $ term v)
definition (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c $ typ t

typ :: Type 'S.Curried -> Type 'S.Uncurried
typ (S.TypeVariable i)      = S.TypeVariable i
typ (S.TypeApplication t s) = S.TypeApplication (typ t) $ typ s
typ (S.FunctionType a)      = S.FunctionType $ functionType a
typ (S.ProcedureType t)     = S.ProcedureType $ typ t

term :: Value 'S.Curried -> Value 'S.Uncurried
term (S.TypedTerm (S.Variable i) t)       = S.TypedTerm (S.Variable i) $ typ t
term (S.TypedTerm (S.Literal l) t)        = S.TypedTerm (S.Literal $ literal l) $ typ t
term (S.TypedTerm (S.Application a) t)    = S.TypedTerm (S.Application $ application a) $ typ t
term (S.TypedTerm (S.Procedure ps) t)     = S.TypedTerm (S.Procedure $ procedureStep <$> ps) $ typ t
term (S.TypedTerm (S.TypeAnnotation _) _) = unreachable
term (S.TypedTerm (S.Let ds v) t)         = S.TypedTerm (S.Let (definition <$> ds) $ term v) $ typ t

literal :: Literal 'S.Curried -> Literal 'S.Uncurried
literal (S.Integer v b)      = S.Integer v b
literal (S.Fraction s f e b) = S.Fraction s f e b
literal (S.String t)         = S.String t
literal (S.Function f)       = S.Function $ function f

functionType :: FunctionType 'S.Curried -> FunctionType 'S.Uncurried
functionType (S.FunctionTypeC t0 (S.FunctionType a)) =
  let
    S.FunctionTypeN ts t = functionType a
    t0' = typ t0
  in S.FunctionTypeN (t0' : ts) t
functionType (S.FunctionTypeC t0 t) = S.FunctionTypeN [typ t0] $ typ t

application :: Application 'S.Curried -> Application 'S.Uncurried
application a =
  S.ApplicationN v vs
  where
    v :| vs = N.reverse $ go a
    go (S.ApplicationC (S.TypedTerm (S.Application a) _) v1) = term v1 :| N.toList (go a)
    go (S.ApplicationC v0 v1)                                = term v1 :| [term v0]

procedureStep :: ProcedureStep 'S.Curried -> ProcedureStep 'S.Uncurried
procedureStep (S.BindProcedure i v) = S.BindProcedure i $ term v
procedureStep (S.TermProcedure v)   = S.TermProcedure $ term v

function :: Function 'S.Curried -> Function 'S.Uncurried
function (S.FunctionC i t (S.TypedTerm (S.Literal (S.Function f)) _)) =
  let
    t' = typ t
    S.FunctionN ps v = function f
  in S.FunctionN ((i, t') : ps) v
function (S.FunctionC i t v) =
  let
    t' = typ t
    v' = term v
  in S.FunctionN [(i, t')] v'
