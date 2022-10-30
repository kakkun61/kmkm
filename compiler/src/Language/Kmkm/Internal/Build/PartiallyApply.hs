{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

-- | “Partial application remove” pass.
module Language.Kmkm.Internal.Build.PartiallyApply
  ( partiallyApply
  ) where

import           Language.Kmkm.Internal.Exception                                               (unreachable)
import qualified Language.Kmkm.Internal.Syntax.Core.Common                                      as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted as S

import           Control.Monad              (replicateM)
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import           Data.Copointed             (Copointed (copoint))
import           Data.Functor.F             (F)
import           Data.Functor.With          (MayHave)
import           Data.Traversable           (for)
import           GHC.Stack                  (HasCallStack)

type Pass = State Word

partiallyApply
  :: ( Traversable f
     , Copointed f
     , MayHave SC.Location f
     , HasCallStack
     )
  => F f (S.Module et ev f)
  -> F f (S.Module et ev f)
partiallyApply = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S.Module et ev f) -> Pass (F f (S.Module et ev f))
module' =
  traverse $ \(S.Module mn ms ds) -> do
      ds' <- mapM (traverse definition) ds
      pure $ S.Module mn ms ds'

definition :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S.Definition et ev f) -> Pass (F f (S.Definition et ev f))
definition =
  traverse $ \case
    S.ValueBind i v -> scope $ S.ValueBind i <$> term v
    d               -> pure d

term :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S.Value et ev f) -> Pass (F f (S.Value et ev f))
term v =
  for v $ \v'@(S.TypedValue v1 t) ->
    case copoint v1 of
      S.Application v2 vs
        | S.TypedValue _ t' <- copoint v2
        , S.FunctionType t0s t0 <- copoint t' -> do
            let
              vs' = copoint vs
              nApp = length vs'
              t0s' = copoint t0s
              nFun = length t0s'
            if nFun < nApp
              then unreachable "# of params < # of args"
              else do
                v2' <- term v2
                vs1 <- mapM term vs'
                if nApp == nFun
                  then
                    pure $ S.TypedValue (S.Application v2' (vs1 <$ vs) <$ v1) t
                  else do -- nApp < nFun
                    let nCls = nFun - nApp
                    is <- replicateM nCls newIdentifier
                    let
                      t0s1 = drop nApp t0s'
                      vs'' = vs1 ++ ((<$ v) <$> (S.TypedValue <$> ((<$ v1) . S.Variable . (<$ v) . SC.LocalIdentifier <$> is) <*> t0s1))
                    pure $
                      S.TypedValue
                        (S.Function (zipWith (\i t -> (i, t) <$ v) ((<$ v) . SC.LocalIdentifier <$> is) t0s1 <$ v) (S.TypedValue (S.Application v2' (vs'' <$ vs) <$ v) t0 <$ v) <$ v)
                        t
      S.Procedure ps -> flip S.TypedValue t . (<$ v1) . S.Procedure <$> mapM (traverse procedureStep) ps
      _ -> pure v'

procedureStep :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S.ProcedureStep et ev f) -> Pass (F f (S.ProcedureStep et ev f))
procedureStep =
  traverse $ \case
    S.BindProcedureStep i v -> S.BindProcedureStep i <$> term v
    S.CallProcedureStep v   -> S.CallProcedureStep <$> term v

newIdentifier :: Pass SC.Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SC.SystemIdentifier 'a' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r
