{-# LANGUAGE DataKinds #-}

-- | “Partial application” pass.
module Language.Kmkm.Build.PartiallyApply
  ( partiallyApply
  , Module
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import           Control.Monad              (replicateM)
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S

type Module = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed

type Definition = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed

type Value = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed

type ProcedureStep = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed

type Pass = State Word

partiallyApply :: Module -> Module
partiallyApply = flip evalState 0 . module'

module' :: Module -> Pass Module
module' (S.Module mn ds ms) = S.Module mn ds <$> sequence (definition mn <$> ms)

definition :: S.ModuleName -> Definition -> Pass Definition
definition mn (S.ValueBind (S.ValueBindU i v)) =
  scope $ do
    v' <- term mn v
    pure $ S.ValueBind (S.ValueBindU i v')
definition _ d = pure d

term :: S.ModuleName -> Value -> Pass Value
term mn (S.TypedTerm (S.Application (S.ApplicationN v@(S.TypedTerm _ (S.FunctionType (S.FunctionTypeN t0s t0))) vs)) t) = do
  let
    nApp = length vs
    nFun = length t0s
  if nFun < nApp
    then unreachable
    else do
      v' <- term mn v
      vs' <- sequence $ term mn <$> vs
      if nApp == nFun
        then
          pure $ S.TypedTerm (S.Application (S.ApplicationN v' vs')) t
        else do -- nApp < nFun
          let nCls = nFun - nApp
          is <- replicateM nCls newIdentifier
          let
            t0s' = drop nApp t0s
            vs'' = vs' ++ (S.TypedTerm . S.Variable <$> (S.LocalIdentifier <$> is) <*> t0s')
          pure $ S.TypedTerm (S.Literal $ S.Function $ S.FunctionN (zip (S.LocalIdentifier <$> is) t0s') (S.TypedTerm (S.Application (S.ApplicationN v' vs'')) t0)) t
term mn (S.TypedTerm (S.Procedure ps) t) =
  flip S.TypedTerm t . S.Procedure <$> sequence (procedureStep mn <$> ps)
term _ v = pure v

procedureStep :: S.ModuleName -> ProcedureStep -> Pass ProcedureStep
procedureStep mn (S.BindProcedure i v) = S.BindProcedure i <$> term mn v
procedureStep mn (S.TermProcedure v)   = S.TermProcedure <$> term mn v

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
