-- | “Lambda lifting” pass.
module Language.Kmkm.Builder.Pass4
  ( lambdaLifting
  ) where

import           Language.Kmkm.Exception     (unreachable)
import           Language.Kmkm.Syntax        (Identifier (SystemIdentifier), ModuleName,
                                              QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax        as S
import qualified Language.Kmkm.Syntax.Phase4 as P4
import qualified Language.Kmkm.Syntax.Phase5 as P5

import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import qualified Data.List.NonEmpty         as N

type Pass = State Word

lambdaLifting :: P4.Module -> P5.Module
lambdaLifting = flip evalState 0 . module'

module' :: P4.Module -> Pass P5.Module
module' (S.Module mn ms ds) = S.Module mn ms <$> sequence (definition mn <$> ds)

definition :: ModuleName -> P4.Definition -> Pass P5.Definition
definition _ (S.DataDefinition i cs) = pure $ S.DataDefinition i cs
definition _ (S.TypeBind i t) = pure $ S.TypeBind i t
definition mn (S.ValueBind (S.BindU i (S.TypedTerm (S.Literal (S.Function (S.FunctionN is v))) _))) =
  scope $ do
    (v'@(S.TypedTerm _ t), ds) <- term mn v
    pure $ S.ValueBind $ S.BindN i is $ S.TypedTerm (S.Let ds v') t
definition mn (S.ValueBind (S.BindU i v)) =
  scope $ do
    (v'@(S.TypedTerm _ t), ds) <- term mn v
    pure $ S.ValueBind $ S.BindV i $ S.TypedTerm (S.Let ds v') t
definition _ (S.ForeignValueBind i hs c t) = pure $ S.ForeignValueBind i hs c t

term :: ModuleName -> P4.Term -> Pass (P5.Term, [P5.Definition])
term _ (S.TypedTerm (S.Variable i) t) = pure (S.TypedTerm (S.Variable i) t, [])
term mn (S.TypedTerm (S.Literal l) t) = do
  (l, ds) <- literal mn l
  pure (S.TypedTerm l t, ds)
term mn (S.TypedTerm (S.Application (S.ApplicationN v vs)) t) = do
  (v', ds) <- term mn v
  (vs', dss) <- unzip <$> sequence (term mn <$> vs)
  pure (S.TypedTerm (S.Application (S.ApplicationN v' vs')) t, mconcat $ ds : dss)
term mn (S.TypedTerm (S.Procedure ps) t) = do
  (ps', dss) <- N.unzip <$> sequence (procedureStep mn <$> ps)
  pure (S.TypedTerm (S.Procedure ps') t, mconcat $ N.toList dss)
term _ (S.TypedTerm S.TypeAnnotation {} _) = unreachable
term mn (S.TypedTerm (S.Let ds v) t) = do
  ds' <- sequence (definition mn <$> ds)
  (v', vds) <- term mn v
  pure (S.TypedTerm (S.Let ds' v') t, vds)

procedureStep :: ModuleName -> P4.ProcedureStep -> Pass (P5.ProcedureStep, [P5.Definition])
procedureStep mn (S.BindProcedure i v) = do
  (v', ds) <- term mn v
  pure (S.BindProcedure i v', ds)
procedureStep mn (S.TermProcedure v) = do
  (v', ds) <- term mn v
  pure (S.TermProcedure v', ds)

literal :: ModuleName -> P4.Literal -> Pass (P5.Term', [P5.Definition])
literal _ (S.Integer v b) = pure (S.Literal $ S.Integer v b, [])
literal _ (S.Fraction s f e b) = pure (S.Literal $ S.Fraction s f e b, [])
literal _ (S.String t) = pure (S.Literal $ S.String t, [])
literal mn (S.Function (S.FunctionN is v)) = do
  i <- newIdentifier
  (v', ds) <- term mn v
  let m = S.ValueBind (S.BindN i is v')
  pure (S.Variable $ QualifiedIdentifier Nothing i, ds ++ [m])

newIdentifier :: Pass Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SystemIdentifier 'l' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r
