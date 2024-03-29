{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | “Partial application remove” pass.
module Language.Kmkm.Internal.Build.PartiallyApply
  ( partiallyApply
  ) where

import           Language.Kmkm.Internal.Exception (unreachable)
import qualified Language.Kmkm.Internal.Syntax    as S

import           Control.Monad              (replicateM)
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import           Data.Copointed             (Copointed (copoint))
import           Data.Traversable           (for)
import Data.Functor.With (MayHave)

type Module et ev f = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f

type Definition et ev f = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f

type Value et ev f = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f

type ProcedureStep et ev f = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f

type Pass = State Word

partiallyApply
  :: ( Traversable f
     , Copointed f
     , MayHave S.Location f
     )
  => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f)
  -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f)
partiallyApply = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, MayHave S.Location f) => f (Module et ev f) -> Pass (f (Module et ev f))
module' =
  traverse $ \(S.Module mn ms ds) -> do
      ds' <- mapM (traverse definition) ds
      pure $ S.Module mn ms ds'

definition :: (Traversable f, Copointed f, MayHave S.Location f) => f (Definition et ev f) -> Pass (f (Definition et ev f))
definition =
  traverse $ \case
    S.ValueBind b -> scope $ S.ValueBind <$> for b (\(S.ValueBindU i v) -> S.ValueBindU i <$> term v)
    d             -> pure d

term :: (Traversable f, Copointed f, MayHave S.Location f) => f (Value et ev f) -> Pass (f (Value et ev f))
term v =
  for v $ \v'@(S.TypedValue v1 t) ->
    case copoint v1 of
      S.Application a
        | S.ApplicationN v2 vs <- copoint a
        , S.TypedValue _ t' <- copoint v2
        , S.FunctionType t'' <- copoint t'
        , S.FunctionTypeN t0s t0 <- copoint t'' -> do
            let
              vs' = copoint vs
              nApp = length vs'
              t0s' = copoint t0s
              nFun = length t0s'
            if nFun < nApp
              then unreachable
              else do
                v2' <- term v2
                vs1 <- mapM term vs'
                if nApp == nFun
                  then
                    pure $ S.TypedValue (S.Application (S.ApplicationN v2' (vs1 <$ vs) <$ a) <$ v1) t
                  else do -- nApp < nFun
                    let nCls = nFun - nApp
                    is <- replicateM nCls newIdentifier
                    let
                      t0s1 = drop nApp t0s'
                      vs'' = vs1 ++ ((<$ v) <$> (S.TypedValue <$> ((<$ v1) . S.Variable . (<$ v) . S.LocalIdentifier <$> is) <*> t0s1))
                    pure $
                      S.TypedValue
                        (S.Function (S.FunctionN (zipWith (\i t -> (i, t) <$ v) ((<$ v) . S.LocalIdentifier <$> is) t0s1 <$ v) (S.TypedValue (S.Application (S.ApplicationN v2' (vs'' <$ vs) <$ v) <$ v) t0 <$ v) <$ v) <$ v)
                        t
      S.Procedure ps -> flip S.TypedValue t . (<$ v1) . S.Procedure <$> mapM (traverse procedureStep) ps
      _ -> pure v'

procedureStep :: (Traversable f, Copointed f, MayHave S.Location f) => f (ProcedureStep et ev f) -> Pass (f (ProcedureStep et ev f))
procedureStep =
  traverse $ \case
    S.BindProcedureStep i v -> S.BindProcedureStep i <$> term v
    S.CallProcedureStep v   -> S.CallProcedureStep <$> term v

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
