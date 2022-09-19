{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | “Lambda lifting” pass.
module Language.Kmkm.Internal.Build.LambdaLift
  ( lambdaLift
  , definition
  , dataRepresentation
  , value
  , Pass
  , peelForAll
  ) where

import qualified Language.Kmkm.Internal.Syntax as S

import           Control.Monad                    (mapAndUnzipM)
import           Control.Monad.State.Strict       (State, evalState)
import qualified Control.Monad.State.Strict       as S
import           Data.Copointed                   (Copointed (copoint))
import           Data.Functor.With                (MayHave)
import qualified Data.List.NonEmpty               as N
import           GHC.Stack                        (HasCallStack)
import qualified Language.Kmkm.Internal.Exception as X

type Module l et ev = S.Module 'S.NameResolved 'S.Uncurried l 'S.Typed et ev

type Definition l et ev = S.Definition 'S.NameResolved 'S.Uncurried l 'S.Typed et ev

type DataRepresentation l et ev = S.DataRepresentation 'S.NameResolved 'S.Uncurried l et ev

type ValueConstructor l et ev = S.ValueConstructor 'S.NameResolved 'S.Uncurried l et ev

type Field l et ev = S.Field 'S.NameResolved 'S.Uncurried l et ev

type Value l et ev = S.Value 'S.NameResolved 'S.Uncurried l 'S.Typed et ev

type ProcedureStep l et ev = S.ProcedureStep 'S.NameResolved 'S.Uncurried l 'S.Typed et ev

type BindIdentifier = S.BindIdentifier 'S.NameResolved

type Pass = State Word

lambdaLift
  :: (Traversable f, Copointed f, MayHave S.Location f, HasCallStack)
  => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev f)
  -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed et ev f)
lambdaLift = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, MayHave S.Location f, HasCallStack) => f (Module 'S.LambdaUnlifted et ev f) -> Pass (f (Module 'S.LambdaLifted et ev f))
module' =
  traverse $ \(S.Module mn ms ds) -> do
    ds' <- mapM (traverse definition) ds
    pure $ S.Module mn ms ds'

definition :: (Traversable f, Copointed f, MayHave S.Location f, HasCallStack) => f (Definition 'S.LambdaUnlifted et ev f) -> Pass (f (Definition 'S.LambdaLifted et ev f))
definition =
  traverse definition'
  where
    definition' (S.DataDefinition i r)    = S.DataDefinition i <$> dataRepresentation r
    definition' (S.TypeBind i t)           = pure $ S.TypeBind i t
    definition' (S.ForeignTypeBind i c)    = pure $ S.ForeignTypeBind i c
    definition' (S.ForeignValueBind i c t) = pure $ S.ForeignValueBind i c t
    definition' (S.ValueBind b) =
      scope $
        let
          S.ValueBindU i v = copoint b
          (is, v1) = peelForAll v
          S.TypedValue v2 t = copoint v1
        in
          case copoint v2 of
            S.Function f -> do
              let S.FunctionN ps v3 = copoint f
              (v3', ds) <- value v3
              let
                S.TypedValue _ t = copoint v3'
                v3'' =
                  case ds of
                    [] -> v3'
                    _  -> S.TypedValue (S.Let (ds <$ v3) v3' <$ v2) t <$ v
              pure $ S.ValueBind $ S.ValueBindN i is ps v3'' <$ b
            S.ForAllValue {} -> X.unreachable "for-all value even after peel"
            _ -> do
              (v1', ds) <- value v1
              let
                v1'' =
                  case ds of
                    [] -> v1'
                    _  -> S.TypedValue (S.Let (ds <$ v) v1' <$ v) t <$ v
              pure $ S.ValueBind $ S.ValueBindV i is v1'' <$ b

dataRepresentation :: Traversable f => f (DataRepresentation 'S.LambdaUnlifted et ev f) -> Pass (f (DataRepresentation 'S.LambdaLifted et ev f))
dataRepresentation = traverse $ \(S.ForAllDataU is cs) -> S.ForAllDataU is <$> traverse (traverse valueConstructor) cs

valueConstructor :: Traversable f => f (ValueConstructor 'S.LambdaUnlifted et ev f) -> Pass (f (ValueConstructor 'S.LambdaLifted et ev f))
valueConstructor = traverse $ \(S.ValueConstructor i fs) -> S.ValueConstructor i <$> traverse (traverse field) fs

field :: Traversable f => f (Field 'S.LambdaUnlifted et ev f) -> Pass (f (Field 'S.LambdaLifted et ev f))
field = traverse $ \(S.Field i t) -> pure $ S.Field i t

value :: (Traversable f, Copointed f, MayHave S.Location f, HasCallStack) => f (Value 'S.LambdaUnlifted et ev f) -> Pass (f (Value 'S.LambdaLifted et ev f), [f (Definition 'S.LambdaLifted et ev f)])
value v =
  case copoint v of
    S.TypedValue v' t ->
      case copoint v' of
        S.Variable i -> pure (S.TypedValue (S.Variable i <$ v') t <$ v, [])
        S.Literal l -> do
          pure (S.TypedValue (S.Literal l <$ v') t <$ v, [])
        S.Function f
          | S.FunctionN ps v'' <- copoint f -> do
              i <- (<$ v) <$> newIdentifier
              (v3, ds) <- value v''
              let (is, v4) = peelForAll v3
              let m = S.ValueBind (S.ValueBindN i is ps v4 <$ v)
              pure (S.TypedValue (S.Variable i <$ v') t <$ v, ds ++ [m <$ v])
        S.Application a
          | S.ApplicationN v1 vs <- copoint a -> do
              (v1', ds) <- value v1
              (vs', dss) <- mapAndUnzipM value (copoint vs)
              pure (S.TypedValue (S.Application (S.ApplicationN v1' (vs' <$ vs) <$ a) <$ v') t <$ v, mconcat $ ds : dss)
        S.Procedure ps -> do
          (ps', dss) <- N.unzip <$> mapM procedureStep (copoint ps)
          pure (S.TypedValue (S.Procedure (ps' <$ ps) <$ v') t <$ v, mconcat $ N.toList dss)
        S.Let ds v1 -> do
          ds' <- mapM (traverse definition) ds
          (v1', vds) <- value v1
          pure (S.TypedValue (S.Let ds' v1' <$ v') t <$ v, vds)
        S.ForAllValue i v1 -> do
          (v1', ds) <- value v1
          pure (S.TypedValue (S.ForAllValue i v1' <$ v') t <$ v, ds)
        S.TypeAnnotation _ -> X.unreachable "type annotation"
        S.Instantiation i -> do
          let S.InstantiationN v ts = copoint i
          (v', ds) <- value v
          pure (S.TypedValue (S.Instantiation (S.InstantiationN v' ts <$ i) <$ v) t <$ v, ds)

procedureStep :: (Traversable f, Copointed f, MayHave S.Location f, HasCallStack) => f (ProcedureStep 'S.LambdaUnlifted et ev f) -> Pass (f (ProcedureStep 'S.LambdaLifted et ev f), [f (Definition 'S.LambdaLifted et ev f)])
procedureStep s =
  case copoint s of
    S.BindProcedureStep i v ->do
      (v', ds) <- value v
      pure (S.BindProcedureStep i v' <$ v, ds)
    S.CallProcedureStep v -> do
      (v', ds) <- value v
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

peelForAll :: (Functor f, Copointed f) => f (Value l et ev f) -> (f [f BindIdentifier], f (Value l et ev f))
peelForAll v =
  case copoint v of
    S.TypedValue v' _ ->
      case copoint v' of
        S.ForAllValue i v'' ->
          let (is, v''') = peelForAll v''
          in ((i : copoint is) <$ v, v''')
        _ -> ([] <$ v, v)
