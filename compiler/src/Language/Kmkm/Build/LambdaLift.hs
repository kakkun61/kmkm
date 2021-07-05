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

type Module l f = S.Module 'S.NameResolved 'S.Uncurried l 'S.Typed f

type Definition l f = S.Definition 'S.NameResolved 'S.Uncurried l 'S.Typed f

type Value l f = S.Value 'S.NameResolved 'S.Uncurried l 'S.Typed f

type Value' l f = S.Value' 'S.NameResolved 'S.Uncurried l 'S.Typed f

type ProcedureStep l f = S.ProcedureStep 'S.NameResolved 'S.Uncurried l 'S.Typed f

type Literal l f = S.Literal 'S.NameResolved 'S.Uncurried l 'S.Typed f

type Pass = State Word

lambdaLift :: S.HasPosition f => S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f -> S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f
lambdaLift = flip evalState 0 . module'

module' :: S.HasPosition f => Module 'S.LambdaUnlifted f -> Pass (Module 'S.LambdaLifted f)
module' (S.Module mn ms ds) = do
  ds' <- sequence $ traverse definition <$> ds
  pure $ S.Module mn ms ds'

definition :: S.HasPosition f => Definition 'S.LambdaUnlifted f -> Pass (Definition 'S.LambdaLifted f)
definition (S.DataDefinition i cs) = pure $ S.DataDefinition i cs
definition (S.TypeBind i t) = pure $ S.TypeBind i t
definition (S.ForeignTypeBind i hs c) = pure $ S.ForeignTypeBind i hs c
definition (S.ValueBind (S.ValueBindU i v)) =
  scope $
    case S.item v of
      S.TypedValue v1 _
        | S.Literal (S.Function (S.FunctionN is v2)) <- S.item v1 -> do
            (v'@(S.TypedValue _ t), ds) <- term $ S.item v2
            pure $ S.ValueBind $ S.ValueBindN i is $ S.TypedValue (S.Let ds (v' <$ v2) <$ v1) t <$ v
      _ -> do
        (v'@(S.TypedValue _ t), ds) <- term $ S.item v
        pure $ S.ValueBind $ S.ValueBindV i $ S.TypedValue (S.Let ds (v' <$ v) <$ v) t <$ v
definition (S.ForeignValueBind i hs c t) = pure $ S.ForeignValueBind i hs c t

term :: S.HasPosition f => Value 'S.LambdaUnlifted f -> Pass (Value 'S.LambdaLifted f, [f (Definition 'S.LambdaLifted f)])
term (S.TypedValue v t) =
  case S.item v of
    S.Variable i -> pure (S.TypedValue (S.Variable i <$ v) t, [])
    S.Literal l -> do
      (l', ds) <- literal l
      pure (S.TypedValue (l' <$ v) t, ds)
    S.Application (S.ApplicationN v1 vs) -> do
      (v1', ds) <- term $ S.item v1
      (vs', dss) <- unzip <$> sequence (term . S.item <$> vs)
      pure (S.TypedValue (S.Application (S.ApplicationN (v1' <$ v1) $ zipWith (<$) vs' vs) <$ v) t, mconcat $ ds : dss)
    S.Procedure ps -> do
      (ps', dss) <- N.unzip <$> sequence (procedureStep . S.item <$> ps)
      pure (S.TypedValue (S.Procedure (N.zipWith (<$) ps' ps) <$ v) t, mconcat $ N.toList dss)
    S.TypeAnnotation {} -> unreachable
    S.Let ds v1 -> do
      ds' <- sequence (traverse definition <$> ds)
      (v1', vds) <- term $ S.item v1
      pure (S.TypedValue (S.Let ds' (v1' <$ v1) <$ v) t, vds)

procedureStep :: S.HasPosition f => ProcedureStep 'S.LambdaUnlifted f -> Pass (ProcedureStep 'S.LambdaLifted f, [f (Definition 'S.LambdaLifted f)])
procedureStep (S.BindProcedure i v) = do
  (v', ds) <- term $ S.item v
  pure (S.BindProcedure i $ v' <$ v, ds)
procedureStep (S.TermProcedure v) = do
  (v', ds) <- term $ S.item v
  pure (S.TermProcedure $ v' <$ v, ds)

literal :: S.HasPosition f => Literal 'S.LambdaUnlifted f -> Pass (Value' 'S.LambdaLifted f, [f (Definition 'S.LambdaLifted f)])
literal (S.Integer v b) = pure (S.Literal $ S.Integer v b, [])
literal (S.Fraction s f e b) = pure (S.Literal $ S.Fraction s f e b, [])
literal (S.String t) = pure (S.Literal $ S.String t, [])
literal (S.Function (S.FunctionN is v)) = do
  i <- newIdentifier
  (v', ds) <- term $ S.item v
  let m = S.ValueBind (S.ValueBindN (i <$ v) is $ v' <$ v) -- i <$ ? は literal 全体がいいかも → 引数も f a
  pure (S.Variable i, ds ++ [m <$ v])

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
