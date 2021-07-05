{-# LANGUAGE DataKinds #-}

-- | “Partial application” pass.
module Language.Kmkm.Build.PartiallyApply
  ( partiallyApply
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import           Control.Monad              (replicateM)
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S

type Module f = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f

type Definition f = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f

type Value f = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f

type ProcedureStep f = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f

type Pass = State Word

partiallyApply :: S.HasPosition f => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f) -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed f)
partiallyApply = fmap $ flip evalState 0 . module'

module' :: S.HasPosition f => Module f -> Pass (Module f)
module' (S.Module mn ms ds) = do
  ds' <- sequence $ traverse definition <$> ds
  pure $ S.Module mn ms ds'

definition :: S.HasPosition f => Definition f -> Pass (Definition f)
definition (S.ValueBind (S.ValueBindU i v)) =
  scope $ do
    v' <- term $ S.item v
    pure $ S.ValueBind (S.ValueBindU i $ v' <$ v)
definition d = pure d

term :: S.HasPosition f => Value f -> Pass (Value f)
term v@(S.TypedValue v1 t) =
  case S.item v1 of
    S.Application (S.ApplicationN v2 vs)
      | S.TypedValue _ t' <- S.item v2
      , S.FunctionType (S.FunctionTypeN t0s t0) <- S.item t' -> do
          let
            nApp = length vs
            nFun = length t0s
          if nFun < nApp
            then unreachable
            else do
              v2' <- traverse term v2
              vs' <- sequence $ traverse term <$> vs
              if nApp == nFun
                then
                  pure $ S.TypedValue (S.Application (S.ApplicationN v2' vs') <$ v1) t
                else do -- nApp < nFun
                  let nCls = nFun - nApp
                  is <- replicateM nCls newIdentifier
                  let
                    t0s' = drop nApp t0s
                    vs'' = vs' ++ ((<$ v1) <$> (S.TypedValue <$> ((<$ v1) . S.Variable . S.LocalIdentifier <$> is) <*> t0s'))
                  pure $
                    S.TypedValue
                      (S.Literal (S.Function $ S.FunctionN (zip ((<$ v1) . S.LocalIdentifier <$> is) t0s') (S.TypedValue (S.Application (S.ApplicationN v2' vs'') <$ v1) t0 <$ v1)) <$ v1)
                      t
    S.Procedure ps -> flip S.TypedValue t . (<$ v1) . S.Procedure <$> sequence (traverse procedureStep <$> ps)
    _ -> pure v

procedureStep :: S.HasPosition f => ProcedureStep f -> Pass (ProcedureStep f)
procedureStep (S.BindProcedure i v) = S.BindProcedure i <$> traverse term v
procedureStep (S.TermProcedure v)   = S.TermProcedure <$> traverse term v

newIdentifier :: Pass S.Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ S.SystemIdentifier 'a' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r
