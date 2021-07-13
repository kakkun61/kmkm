{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

-- | “Lambda lifting” pass.
module Language.Kmkm.Build.LambdaLift
  ( lambdaLift
  ) where

import qualified Language.Kmkm.Syntax as S

import qualified Barbies.Bare               as B
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S
import           Data.Copointed             (Copointed (copoint))
import qualified Data.List.NonEmpty         as N

type Module l f = S.Module 'S.NameResolved 'S.Uncurried l 'S.Typed B.Covered f

type Definition l f = S.Definition 'S.NameResolved 'S.Uncurried l 'S.Typed B.Covered f

type Value l f = S.Value 'S.NameResolved 'S.Uncurried l 'S.Typed B.Covered f

type ProcedureStep l f = S.ProcedureStep 'S.NameResolved 'S.Uncurried l 'S.Typed B.Covered f

type Pass = State Word

lambdaLift :: (Traversable f, Copointed f, S.HasPosition f) => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f) -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed B.Covered f)
lambdaLift = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, S.HasPosition f) => f (Module 'S.LambdaUnlifted f) -> Pass (f (Module 'S.LambdaLifted f))
module' =
  traverse $ \(S.Module mn ms ds) -> do
    ds' <- sequence $ traverse definition <$> ds
    pure $ S.Module mn ms ds'

definition :: (Traversable f, Copointed f, S.HasPosition f) => f (Definition 'S.LambdaUnlifted f) -> Pass (f (Definition 'S.LambdaLifted f))
definition =
  traverse $ \case
    S.DataDefinition i cs -> pure $ S.DataDefinition i cs
    S.TypeBind i t -> pure $ S.TypeBind i t
    S.ForeignTypeBind i hs c -> pure $ S.ForeignTypeBind i hs c
    S.ValueBind (S.ValueBindU i v) ->
      scope $
        case copoint v of
          S.TypedValue v1 _
            | S.Function (S.FunctionN is v2) <- copoint v1 -> do
                (v', ds) <- term v2
                let S.TypedValue _ t = copoint v'
                pure $ S.ValueBind $ S.ValueBindN i is $ S.TypedValue (S.Let (ds <$ v2) v' <$ v1) t <$ v
          _ -> do
            (v', ds) <- term v
            let S.TypedValue _ t = copoint v'
            pure $ S.ValueBind $ S.ValueBindV i $ S.TypedValue (S.Let (ds <$ v) v' <$ v) t <$ v
    S.ForeignValueBind i hs c t -> pure $ S.ForeignValueBind i hs c t

term :: (Traversable f, Copointed f, S.HasPosition f) =>f (Value 'S.LambdaUnlifted f) -> Pass (f (Value 'S.LambdaLifted f), [f (Definition 'S.LambdaLifted f)])
term v =
  case copoint v of
    S.TypedValue v' t ->
      case copoint v' of
        S.Variable i -> pure (S.TypedValue (S.Variable i <$ v') t <$ v, [])
        S.Literal l -> do
          pure (S.TypedValue (S.Literal l <$ v') t <$ v, [])
        S.Function (S.FunctionN is v'') -> do
          i <- (<$ v) <$> newIdentifier
          (v''', ds) <- term v''
          let m = S.ValueBind (S.ValueBindN i is v''')
          pure (S.TypedValue (S.Variable i <$ v') t <$ v, ds ++ [m <$ v])
        S.Application (S.ApplicationN v1 vs) -> do
          (v1', ds) <- term v1
          (vs', dss) <- unzip <$> sequence (term <$> copoint vs)
          pure (S.TypedValue (S.Application (S.ApplicationN v1' $ vs' <$ vs) <$ v') t <$ v, mconcat $ ds : dss)
        S.Procedure ps -> do
          (ps', dss) <- N.unzip <$> sequence (procedureStep <$> copoint ps)
          pure (S.TypedValue (S.Procedure (ps' <$ ps) <$ v') t <$ v, mconcat $ N.toList dss)
        S.Let ds v1 -> do
          ds' <- sequence (traverse definition <$> ds)
          (v1', vds) <- term v1
          pure (S.TypedValue (S.Let ds' v1' <$ v') t <$ v, vds)

procedureStep :: (Traversable f, Copointed f, S.HasPosition f) => f (ProcedureStep 'S.LambdaUnlifted f) -> Pass (f (ProcedureStep 'S.LambdaLifted f), [f (Definition 'S.LambdaLifted f)])
procedureStep s =
  case copoint s of
    S.BindProcedure i v ->do
      (v', ds) <- term v
      pure (S.BindProcedure i v' <$ v, ds)
    S.TermProcedure v -> do
      (v', ds) <- term v
      pure (S.TermProcedure v' <$ v, ds)

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
