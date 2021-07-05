{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Build.Uncurry
  ( uncurry
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Prelude            (Functor (fmap), ($), (<$>))

type Module c f = S.Module 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Definition c f = S.Definition 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Value c f = S.Value 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Value' c f = S.Value' 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type ProcedureStep c f = S.ProcedureStep 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Literal c f = S.Literal 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Application c f = S.Application 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Function c f = S.Function 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed f

type Type c f = S.Type 'S.NameResolved c f

type FunctionType c f = S.FunctionType 'S.NameResolved c f

uncurry :: S.HasPosition f => f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed f) -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f)
uncurry = fmap module'

module' :: S.HasPosition f => Module 'S.Curried f -> Module 'S.Uncurried f
module' (S.Module i ds ms) = S.Module i ds $ fmap definition <$> ms

definition :: S.HasPosition f => Definition 'S.Curried f -> Definition 'S.Uncurried f
definition (S.DataDefinition i cs) =
  S.DataDefinition i $ go <$> cs
  where
    go (i, fs) =
      (i, go <$> fs)
      where
        go (i, t) = (i, typ <$> t)
definition (S.TypeBind i t) = S.TypeBind i $ typ <$> t
definition (S.ForeignTypeBind i hs c) = S.ForeignTypeBind i hs c
definition (S.ValueBind (S.ValueBindU i v)) = S.ValueBind (S.ValueBindU i $ typedValue <$> v)
definition (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c $ typ <$> t

typ :: S.HasPosition f => Type 'S.Curried f -> Type 'S.Uncurried f
typ (S.TypeVariable i)      = S.TypeVariable i
typ (S.TypeApplication t s) = S.TypeApplication (typ <$> t) $ typ <$> s
typ (S.FunctionType a)      = S.FunctionType $ functionType a
typ (S.ProcedureType t)     = S.ProcedureType $ typ <$> t

typedValue :: S.HasPosition f => Value 'S.Curried f -> Value 'S.Uncurried f
typedValue (S.TypedValue v t)       = S.TypedValue (value' <$> v) $ typ <$> t

value' :: S.HasPosition f => Value' 'S.Curried f -> Value' 'S.Uncurried f
value' (S.Variable i)       = S.Variable i
value' (S.Literal l)        = S.Literal $ literal l
value' (S.Application a)    = S.Application $ application a
value' (S.Procedure ps)     = S.Procedure $ fmap procedureStep <$> ps
value' (S.TypeAnnotation _) = unreachable
value' (S.Let ds v)         = S.Let (fmap definition <$> ds) $ typedValue <$> v

literal :: S.HasPosition f => Literal 'S.Curried f -> Literal 'S.Uncurried f
literal (S.Integer v b)      = S.Integer v b
literal (S.Fraction s f e b) = S.Fraction s f e b
literal (S.String t)         = S.String t
literal (S.Function f)       = S.Function $ function f

functionType :: S.HasPosition f => FunctionType 'S.Curried f -> FunctionType 'S.Uncurried f
functionType (S.FunctionTypeC t0 t) | S.FunctionType a <- S.item t =
  let
    S.FunctionTypeN ts t = functionType a
    t0' = typ <$> t0
  in S.FunctionTypeN (t0' : ts) t
functionType (S.FunctionTypeC t0 t) = S.FunctionTypeN [typ <$> t0] $ typ <$> t

application :: S.HasPosition f => Application 'S.Curried f -> Application 'S.Uncurried f
application a =
  S.ApplicationN v vs
  where
    v :| vs = N.reverse $ go a
    go (S.ApplicationC v0 v1)
      | S.TypedValue v0' _ <- S.item v0
      , S.Application a <- S.item v0' = fmap typedValue v1 :| N.toList (go a)
    go (S.ApplicationC v0 v1)                                = fmap typedValue v1 :| [typedValue <$> v0]

procedureStep :: S.HasPosition f => ProcedureStep 'S.Curried f -> ProcedureStep 'S.Uncurried f
procedureStep (S.BindProcedure i v) = S.BindProcedure i $ typedValue <$> v
procedureStep (S.TermProcedure v)   = S.TermProcedure $ typedValue <$> v

function :: S.HasPosition f => Function 'S.Curried f -> Function 'S.Uncurried f
function (S.FunctionC i t v)
  | S.TypedValue v' _ <- S.item v
  , S.Literal (S.Function f) <- S.item v' =
  let
    t' = typ <$> t
    S.FunctionN ps v = function f
  in S.FunctionN ((i, t') : ps) v
function (S.FunctionC i t v) =
  let
    t' = typ <$> t
    v' = typedValue <$> v
  in S.FunctionN [(i, t')] v'
