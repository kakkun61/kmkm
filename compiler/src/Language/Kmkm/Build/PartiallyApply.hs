{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

-- | “Partial application remove” pass.
module Language.Kmkm.Build.PartiallyApply
  ( partiallyApply
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import qualified Barbies.Bare               as B
import           Control.Monad              (replicateM)
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import           Data.Copointed             (Copointed (copoint))
import           Data.Traversable           (for)

type Module f = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f

type Definition f = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f

type Value f = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f

type ProcedureStep f = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f

type Pass = State Word

partiallyApply :: (Traversable f, Copointed f, S.HasPosition f) => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f) -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f)
partiallyApply = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, S.HasPosition f) => f (Module f) -> Pass (f (Module f))
module' =
  traverse $ \(S.Module mn ms ds) -> do
      ds' <- sequence $ traverse definition <$> ds
      pure $ S.Module mn ms ds'

definition :: (Traversable f, Copointed f, S.HasPosition f) => f (Definition f) -> Pass (f (Definition f))
definition =
  traverse $ \case
    S.ValueBind (S.ValueBindU i v) -> scope $ S.ValueBind . S.ValueBindU i <$> term v
    d                              -> pure d

term :: (Traversable f, Copointed f, S.HasPosition f) => f (Value f) -> Pass (f (Value f))
term v =
  for v $ \v'@(S.TypedValue v1 t) ->
    case copoint v1 of
      S.Application (S.ApplicationN v2 vs)
        | S.TypedValue _ t' <- copoint v2
        , S.FunctionType (S.FunctionTypeN t0s t0) <- copoint t' -> do
            let
              vs' = copoint vs
              nApp = length vs'
              t0s' = copoint t0s
              nFun = length t0s'
            if nFun < nApp
              then unreachable
              else do
                v2' <- term v2
                vs1 <- sequence $ term <$> vs'
                if nApp == nFun
                  then
                    pure $ S.TypedValue (S.Application (S.ApplicationN v2' (vs1 <$ vs)) <$ v1) t
                  else do -- nApp < nFun
                    let nCls = nFun - nApp
                    is <- replicateM nCls newIdentifier
                    let
                      t0s1 = drop nApp t0s'
                      vs'' = vs1 ++ ((<$ v) <$> (S.TypedValue <$> ((<$ v1) . S.Variable . (<$ v) . S.LocalIdentifier <$> is) <*> t0s1))
                    pure $
                      S.TypedValue
                        (S.Literal (S.Function $ S.FunctionN (zipWith (\i t -> (i, t) <$ v) ((<$ v) . S.LocalIdentifier <$> is) t0s1 <$ v) (S.TypedValue (S.Application (S.ApplicationN v2' (vs'' <$ vs)) <$ v) t0 <$ v)) <$ v)
                        t
      S.Procedure ps -> flip S.TypedValue t . (<$ v1) . S.Procedure <$> sequence (traverse procedureStep <$> ps)
      _ -> pure v'

procedureStep :: (Traversable f, Copointed f, S.HasPosition f) => f (ProcedureStep f) -> Pass (f (ProcedureStep f))
procedureStep =
  traverse $ \case
    S.BindProcedure i v -> S.BindProcedure i <$> term v
    S.TermProcedure v   -> S.TermProcedure <$> term v

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
