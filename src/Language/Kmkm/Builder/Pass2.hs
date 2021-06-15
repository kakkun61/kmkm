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
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Prelude            (($), (<$>))

uncurry :: P2.Module -> P3.Module
uncurry = module'

module' :: P2.Module -> P3.Module
module' (S.Module i ms) = S.Module i $ member <$> ms

member :: P2.Member -> P3.Member
member (S.Definition i cs) =
  S.Definition i $ go <$> cs
  where
    go (i, fs) =
      (i, go <$> fs)
      where
        go (i, t) = (i, typ t)
member (S.TypeBind i t) = S.TypeBind i $ typ t
member (S.ValueBind (S.ValueBindU i v) ms) = S.ValueBind (S.ValueBindU i $ term v) $ member <$> ms
member (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c $ typ t

typ :: P2.Type -> P3.Type
typ (T.Variable i)      = T.Variable i
typ (T.Application t s) = T.Application (typ t) $ typ s
typ (T.Function a)      = T.Function $ arrow a
typ (T.Procedure t)     = T.Procedure $ typ t

term :: P2.Term -> P3.Term
term (V.TypedTerm (V.Variable i) t)       = V.TypedTerm (V.Variable i) $ typ t
term (V.TypedTerm (V.Literal l) t)        = V.TypedTerm (V.Literal $ literal l) $ typ t
term (V.TypedTerm (V.Application a) t)    = V.TypedTerm (V.Application $ application a) $ typ t
term (V.TypedTerm (V.Procedure ps) t)     = V.TypedTerm (V.Procedure $ procedureStep <$> ps) $ typ t
term (V.TypedTerm (V.TypeAnnotation _) _) = unreachable

literal :: P2.Literal -> P3.Literal
literal (V.Integer v b)      = V.Integer v b
literal (V.Fraction s f e b) = V.Fraction s f e b
literal (V.String t)         = V.String t
literal (V.Function f)       = V.Function $ function f

arrow :: P2.TFunction -> P3.TFunction
arrow (T.FunctionC t0 (T.Function a)) =
  let
    T.FunctionN ts t = arrow a
    t0' = typ t0
  in T.FunctionN (t0' : ts) t
arrow (T.FunctionC t0 t) = T.FunctionN [typ t0] $ typ t

application :: P2.Application -> P3.Application
application a =
  V.ApplicationN v vs
  where
    v :| vs = N.reverse $ go a
    go (V.ApplicationC (V.TypedTerm (V.Application a) _) v1) = term v1 :| N.toList (go a)
    go (V.ApplicationC v0 v1)                                = term v1 :| [term v0]

procedureStep :: P2.ProcedureStep -> P3.ProcedureStep
procedureStep (V.BindProcedure i v) = V.BindProcedure i $ term v
procedureStep (V.TermProcedure v)   = V.TermProcedure $ term v

function :: P2.Function -> P3.Function
function (V.FunctionC i t (V.TypedTerm (V.Literal (V.Function f)) _)) =
  let
    t' = typ t
    V.FunctionN ps v = function f
  in V.FunctionN ((i, t') : ps) v
function (V.FunctionC i t v) =
  let
    t' = typ t
    v' = term v
  in V.FunctionN [(i, t')] v'
