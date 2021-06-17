-- | \"Lambda lifting\" pass. And “top level identifier qualifying”.
module Language.Kmkm.Builder.Pass4
  ( lambdaLifting
  ) where

import           Language.Kmkm.Exception     (unreachable)
import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier (SystemIdentifier), ModuleName,
                                              QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax.Phase4 as P4
import qualified Language.Kmkm.Syntax.Phase5 as P5
import qualified Language.Kmkm.Syntax.Value  as V

import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import qualified Data.List.NonEmpty         as N

type Pass = State Word

lambdaLifting :: P4.Module -> P5.Module
lambdaLifting = flip evalState 0 . module'

module' :: P4.Module -> Pass P5.Module
module' (S.Module mn ds ms) = S.Module mn ds <$> sequence (member mn <$> ms)

member :: ModuleName -> P4.Member -> Pass P5.Member
member _ (S.Definition i cs) = pure $ S.Definition i cs
member _ (S.TypeBind i t) = pure $ S.TypeBind i t
member mn (S.ValueBind b ms) =
  scope $ do
    ms' <- sequence $ member mn <$> ms
    (b, ms'') <- valueBind mn b
    pure $ S.ValueBind b (ms' ++ ms'')
member _ (S.ForeignValueBind i hs c t) = pure $ S.ForeignValueBind i hs c t

valueBind :: ModuleName -> P4.ValueBind -> Pass (P5.ValueBind, [P5.Member])
valueBind mn (S.ValueBindU i (V.TypedTerm (V.Literal (V.Function (V.FunctionN is v))) _)) = do
  (v', ms) <- term mn v
  pure (S.ValueBindN i is v', ms)
valueBind mn (S.ValueBindU i v) = do
  (v', ms) <- term mn v
  pure (S.ValueBindV i v', ms)

term :: ModuleName -> P4.Term -> Pass (P5.Term, [P5.Member])
term _ (V.TypedTerm (V.Variable i) t) = pure (V.TypedTerm (V.Variable i) t, [])
term mn (V.TypedTerm (V.Literal l) t) = do
  (l, ms) <- literal mn l
  pure (V.TypedTerm l t, ms)
term mn (V.TypedTerm (V.Application (V.ApplicationN v vs)) t) = do
  (v', ms) <- term mn v
  (vs', mss) <- unzip <$> sequence (term mn <$> vs)
  pure (V.TypedTerm (V.Application (V.ApplicationN v' vs')) t, mconcat $ ms : mss)
term mn (V.TypedTerm (V.Procedure ps) t) = do
  (ps', mss) <- N.unzip <$> sequence (procedureStep mn <$> ps)
  pure (V.TypedTerm (V.Procedure ps') t, mconcat $ N.toList mss)
term _ (V.TypedTerm V.TypeAnnotation {} _) = unreachable

procedureStep :: ModuleName -> P4.ProcedureStep -> Pass (P5.ProcedureStep, [P5.Member])
procedureStep mn (V.BindProcedure i v) = do
  (v', ms) <- term mn v
  pure (V.BindProcedure i v', ms)
procedureStep mn (V.TermProcedure v) = do
  (v', ms) <- term mn v
  pure (V.TermProcedure v', ms)

literal :: ModuleName -> P4.Literal -> Pass (P5.Term', [P5.Member])
literal _ (V.Integer v b) = pure (V.Literal $ V.Integer v b, [])
literal _ (V.Fraction s f e b) = pure (V.Literal $ V.Fraction s f e b, [])
literal _ (V.String t) = pure (V.Literal $ V.String t, [])
literal mn (V.Function (V.FunctionN is v)) = do
  i <- newIdentifier
  (v', ms) <- term mn v
  let m = S.ValueBind (S.ValueBindN i is v') ms
  pure (V.Variable $ QualifiedIdentifier Nothing i, [m])

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
