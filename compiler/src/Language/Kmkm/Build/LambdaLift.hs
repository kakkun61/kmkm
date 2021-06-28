{-# LANGUAGE DataKinds #-}

-- | “Lambda lifting” pass.
module Language.Kmkm.Build.LambdaLift
  ( lambdaLift
  , Module
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import qualified Data.List.NonEmpty         as N

type Module l = S.Module 'S.NameResolved 'S.Uncurried l 'S.Typed

type Definition l = S.Definition 'S.NameResolved 'S.Uncurried l 'S.Typed

type Value l = S.Value 'S.NameResolved 'S.Uncurried l 'S.Typed

type Value' l = S.Value' 'S.NameResolved 'S.Uncurried l 'S.Typed

type ProcedureStep l = S.ProcedureStep 'S.NameResolved 'S.Uncurried l 'S.Typed

type Literal l = S.Literal 'S.NameResolved 'S.Uncurried l 'S.Typed

type Pass = State Word

lambdaLift :: Module 'S.LambdaUnlifted -> Module 'S.LambdaLifted
lambdaLift = flip evalState 0 . module'

module' :: Module 'S.LambdaUnlifted -> Pass (Module 'S.LambdaLifted)
module' (S.Module mn ms ds) = S.Module mn ms <$> sequence (definition <$> ds)

definition :: Definition 'S.LambdaUnlifted -> Pass (Definition 'S.LambdaLifted)
definition (S.DataDefinition i cs) = pure $ S.DataDefinition i cs
definition (S.TypeBind i t) = pure $ S.TypeBind i t
definition (S.ValueBind (S.ValueBindU i (S.TypedTerm (S.Literal (S.Function (S.FunctionN is v))) _))) =
  scope $ do
    (v'@(S.TypedTerm _ t), ds) <- term v
    pure $ S.ValueBind $ S.ValueBindN i is $ S.TypedTerm (S.Let ds v') t
definition (S.ValueBind (S.ValueBindU i v)) =
  scope $ do
    (v'@(S.TypedTerm _ t), ds) <- term v
    pure $ S.ValueBind $ S.ValueBindV i $ S.TypedTerm (S.Let ds v') t
definition (S.ForeignValueBind i hs c t) = pure $ S.ForeignValueBind i hs c t

term :: Value 'S.LambdaUnlifted -> Pass (Value 'S.LambdaLifted, [Definition 'S.LambdaLifted])
term (S.TypedTerm (S.Variable i) t) = pure (S.TypedTerm (S.Variable i) t, [])
term (S.TypedTerm (S.Literal l) t) = do
  (l, ds) <- literal l
  pure (S.TypedTerm l t, ds)
term (S.TypedTerm (S.Application (S.ApplicationN v vs)) t) = do
  (v', ds) <- term v
  (vs', dss) <- unzip <$> sequence (term <$> vs)
  pure (S.TypedTerm (S.Application (S.ApplicationN v' vs')) t, mconcat $ ds : dss)
term (S.TypedTerm (S.Procedure ps) t) = do
  (ps', dss) <- N.unzip <$> sequence (procedureStep <$> ps)
  pure (S.TypedTerm (S.Procedure ps') t, mconcat $ N.toList dss)
term (S.TypedTerm S.TypeAnnotation {} _) = unreachable
term (S.TypedTerm (S.Let ds v) t) = do
  ds' <- sequence (definition <$> ds)
  (v', vds) <- term v
  pure (S.TypedTerm (S.Let ds' v') t, vds)

procedureStep :: ProcedureStep 'S.LambdaUnlifted -> Pass (ProcedureStep 'S.LambdaLifted, [Definition 'S.LambdaLifted])
procedureStep (S.BindProcedure i v) = do
  (v', ds) <- term v
  pure (S.BindProcedure i v', ds)
procedureStep (S.TermProcedure v) = do
  (v', ds) <- term v
  pure (S.TermProcedure v', ds)

literal :: Literal 'S.LambdaUnlifted -> Pass (Value' 'S.LambdaLifted, [Definition 'S.LambdaLifted])
literal (S.Integer v b) = pure (S.Literal $ S.Integer v b, [])
literal (S.Fraction s f e b) = pure (S.Literal $ S.Fraction s f e b, [])
literal (S.String t) = pure (S.Literal $ S.String t, [])
literal (S.Function (S.FunctionN is v)) = do
  i <- newIdentifier
  (v', ds) <- term v
  let m = S.ValueBind (S.ValueBindN i is v')
  pure (S.Variable i, ds ++ [m])

newIdentifier :: Pass S.QualifiedIdentifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ S.LocalIdentifier $ S.SystemIdentifier 'l' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r
