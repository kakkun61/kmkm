{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Builder.Pass2
  ( uncurry
  ) where

import           Language.Kmkm.Exception     (unreachable)
import qualified Language.Kmkm.Syntax        as S
import qualified Language.Kmkm.Syntax.Phase2 as P2
import qualified Language.Kmkm.Syntax.Phase3 as P3

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Prelude            (($), (<$>))

uncurry :: P2.Module -> P3.Module
uncurry = module'

module' :: P2.Module -> P3.Module
module' (S.Module i ds ms) = S.Module i ds $ definition <$> ms

definition :: P2.Definition -> P3.Definition
definition (S.DataDefinition i cs) =
  S.DataDefinition i $ go <$> cs
  where
    go (i, fs) =
      (i, go <$> fs)
      where
        go (i, t) = (i, typ t)
definition (S.TypeBind i t) = S.TypeBind i $ typ t
definition (S.ValueBind (S.BindU i v)) = S.ValueBind (S.BindU i $ term v)
definition (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c $ typ t

typ :: P2.Type -> P3.Type
typ (S.TypeVariable i)      = S.TypeVariable i
typ (S.TypeApplication t s) = S.TypeApplication (typ t) $ typ s
typ (S.FunctionType a)      = S.FunctionType $ functionType a
typ (S.ProcedureType t)     = S.ProcedureType $ typ t

term :: P2.Term -> P3.Term
term (S.TypedTerm (S.Variable i) t)       = S.TypedTerm (S.Variable i) $ typ t
term (S.TypedTerm (S.Literal l) t)        = S.TypedTerm (S.Literal $ literal l) $ typ t
term (S.TypedTerm (S.Application a) t)    = S.TypedTerm (S.Application $ application a) $ typ t
term (S.TypedTerm (S.Procedure ps) t)     = S.TypedTerm (S.Procedure $ procedureStep <$> ps) $ typ t
term (S.TypedTerm (S.TypeAnnotation _) _) = unreachable
term (S.TypedTerm (S.Let ds v) t)         = S.TypedTerm (S.Let (definition <$> ds) $ term v) $ typ t

literal :: P2.Literal -> P3.Literal
literal (S.Integer v b)      = S.Integer v b
literal (S.Fraction s f e b) = S.Fraction s f e b
literal (S.String t)         = S.String t
literal (S.Function f)       = S.Function $ function f

functionType :: P2.FunctionType -> P3.FunctionType
functionType (S.FunctionTypeC t0 (S.FunctionType a)) =
  let
    S.FunctionTypeN ts t = functionType a
    t0' = typ t0
  in S.FunctionTypeN (t0' : ts) t
functionType (S.FunctionTypeC t0 t) = S.FunctionTypeN [typ t0] $ typ t

application :: P2.Application -> P3.Application
application a =
  S.ApplicationN v vs
  where
    v :| vs = N.reverse $ go a
    go (S.ApplicationC (S.TypedTerm (S.Application a) _) v1) = term v1 :| N.toList (go a)
    go (S.ApplicationC v0 v1)                                = term v1 :| [term v0]

procedureStep :: P2.ProcedureStep -> P3.ProcedureStep
procedureStep (S.BindProcedure i v) = S.BindProcedure i $ term v
procedureStep (S.TermProcedure v)   = S.TermProcedure $ term v

function :: P2.Function -> P3.Function
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
