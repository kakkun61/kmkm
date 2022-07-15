{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | “Lambda lifting” pass.
module Language.Kmkm.Internal.Build.LambdaLift
  ( lambdaLift
  ) where

import qualified Language.Kmkm.Internal.Syntax as S

import qualified Barbies.Bare                     as B
import           Control.Monad                    (mapAndUnzipM)
import           Control.Monad.State.Strict       (State, evalState)
import qualified Control.Monad.State.Strict       as S
import           Data.Copointed                   (Copointed (copoint))
import qualified Data.List.NonEmpty               as N
import qualified Language.Kmkm.Internal.Exception as X

type Module l et ev = S.Module 'S.NameResolved 'S.Uncurried l 'S.Typed et ev B.Covered

type Definition l et ev = S.Definition 'S.NameResolved 'S.Uncurried l 'S.Typed et ev B.Covered

type ValueConstructor l et ev = S.ValueConstructor 'S.NameResolved 'S.Uncurried l et ev B.Covered

type Field l et ev = S.Field 'S.NameResolved 'S.Uncurried l et ev B.Covered

type Value l et ev = S.Value 'S.NameResolved 'S.Uncurried l 'S.Typed et ev B.Covered

type ProcedureStep l et ev = S.ProcedureStep 'S.NameResolved 'S.Uncurried l 'S.Typed et ev B.Covered

type Pass = State Word

lambdaLift :: (Traversable f, Copointed f, S.HasLocation f) => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev B.Covered f) -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed et ev B.Covered f)
lambdaLift = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, S.HasLocation f) => f (Module 'S.LambdaUnlifted et ev f) -> Pass (f (Module 'S.LambdaLifted et ev f))
module' =
  traverse $ \(S.Module mn ms ds) -> do
    ds' <- mapM (traverse definition) ds
    pure $ S.Module mn ms ds'

definition :: (Traversable f, Copointed f, S.HasLocation f) => f (Definition 'S.LambdaUnlifted et ev f) -> Pass (f (Definition 'S.LambdaLifted et ev f))
definition =
  traverse definition'
  where
    definition' (S.DataDefinition i cs)    = S.DataDefinition i <$> traverse (traverse valueConstructor) cs
    definition' (S.TypeBind i t)           = pure $ S.TypeBind i t
    definition' (S.ForeignTypeBind i c)    = pure $ S.ForeignTypeBind i c
    definition' (S.ForeignValueBind i c t) = pure $ S.ForeignValueBind i c t
    definition' (S.ValueBind (S.ValueBindU i v)) =
      scope $
        case copoint v of
          S.TypedValue v1 _
            | S.Function f <- copoint v1
            , S.FunctionN is v2 <- copoint f -> do
                (v', ds) <- term v2
                let S.TypedValue _ t = copoint v'
                pure $ S.ValueBind $ S.ValueBindN i is $ S.TypedValue (S.Let (ds <$ v2) v' <$ v1) t <$ v
            | S.ForAll _ v1' <- copoint v1 -> definition' $ S.ValueBind $ S.ValueBindU i v1'
          _ -> do
            (v', ds) <- term v
            let S.TypedValue _ t = copoint v'
            pure $ S.ValueBind $ S.ValueBindV i $ S.TypedValue (S.Let (ds <$ v) v' <$ v) t <$ v

valueConstructor :: Traversable f => f (ValueConstructor 'S.LambdaUnlifted et ev f) -> Pass (f (ValueConstructor 'S.LambdaLifted et ev f))
valueConstructor = traverse $ \(S.ValueConstructor i fs) -> S.ValueConstructor i <$> traverse (traverse field) fs

field :: Traversable f => f (Field 'S.LambdaUnlifted et ev f) -> Pass (f (Field 'S.LambdaLifted et ev f))
field = traverse $ \(S.Field i t) -> pure $ S.Field i t

term :: (Traversable f, Copointed f, S.HasLocation f) => f (Value 'S.LambdaUnlifted et ev f) -> Pass (f (Value 'S.LambdaLifted et ev f), [f (Definition 'S.LambdaLifted et ev f)])
term v =
  case copoint v of
    S.TypedValue v' t ->
      case copoint v' of
        S.Variable i -> pure (S.TypedValue (S.Variable i <$ v') t <$ v, [])
        S.Literal l -> do
          pure (S.TypedValue (S.Literal l <$ v') t <$ v, [])
        S.Function f
          | S.FunctionN is v'' <- copoint f -> do
              i <- (<$ v) <$> newIdentifier
              (v''', ds) <- term v''
              let m = S.ValueBind (S.ValueBindN i is v''')
              pure (S.TypedValue (S.Variable i <$ v') t <$ v, ds ++ [m <$ v])
        S.Application a
          | S.ApplicationN v1 vs <- copoint a -> do
              (v1', ds) <- term v1
              (vs', dss) <- mapAndUnzipM term (copoint vs)
              pure (S.TypedValue (S.Application (S.ApplicationN v1' (vs' <$ vs) <$ a) <$ v') t <$ v, mconcat $ ds : dss)
        S.Procedure ps -> do
          (ps', dss) <- N.unzip <$> mapM procedureStep (copoint ps)
          pure (S.TypedValue (S.Procedure (ps' <$ ps) <$ v') t <$ v, mconcat $ N.toList dss)
        S.Let ds v1 -> do
          ds' <- mapM (traverse definition) ds
          (v1', vds) <- term v1
          pure (S.TypedValue (S.Let ds' v1' <$ v') t <$ v, vds)
        S.ForAll i v1 -> do
          (v1', ds) <- term v1
          pure (S.TypedValue (S.ForAll i v1' <$ v') t <$ v, ds)
        S.TypeAnnotation _ -> X.unreachable

procedureStep :: (Traversable f, Copointed f, S.HasLocation f) => f (ProcedureStep 'S.LambdaUnlifted et ev f) -> Pass (f (ProcedureStep 'S.LambdaLifted et ev f), [f (Definition 'S.LambdaLifted et ev f)])
procedureStep s =
  case copoint s of
    S.BindProcedureStep i v ->do
      (v', ds) <- term v
      pure (S.BindProcedureStep i v' <$ v, ds)
    S.CallProcedureStep v -> do
      (v', ds) <- term v
      pure (S.CallProcedureStep v' <$ v, ds)

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
