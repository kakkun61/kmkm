{-# LANGUAGE FlexibleContexts #-}

-- | “Lambda lifting” pass.
module Language.Kmkm.Internal.Build.LambdaLift
  ( lambdaLift
  , definition
  , dataRepresentation
  , value
  , Pass
  , peelForAll4
  , peelForAll5
  ) where

import qualified Language.Kmkm.Internal.Syntax.Core.Common                                      as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaLifted   as S5
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted as S4

import           Control.Monad                    (mapAndUnzipM)
import           Control.Monad.State.Strict       (State, evalState)
import qualified Control.Monad.State.Strict       as S
import           Data.Copointed                   (Copointed (copoint))
import           Data.Functor.F                   (F)
import           Data.Functor.With                (MayHave)
import qualified Data.List.NonEmpty               as N
import           GHC.Stack                        (HasCallStack)
import qualified Language.Kmkm.Internal.Exception as X

type Pass = State Word

lambdaLift
  :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack)
  => F f (S4.Module et ev f)
  -> F f (S5.Module et ev f)
lambdaLift = flip evalState 0 . module'

module' :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S4.Module et ev f) -> Pass (F f (S5.Module et ev f))
module' =
  traverse $ \(S4.Module mn ms ds) -> do
    ds' <- mapM (traverse definition) ds
    pure $ S5.Module mn ms ds'

definition :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S4.Definition et ev f) -> Pass (F f (S5.Definition et ev f))
definition =
  traverse definition'
  where
    definition' (S4.DataDefinition i r)    = S5.DataDefinition i <$> dataRepresentation r
    definition' (S4.TypeBind i t)           = pure $ S5.TypeBind i t
    definition' (S4.ForeignTypeBind i c)    = pure $ S5.ForeignTypeBind i c
    definition' (S4.ForeignValueBind i c t) = pure $ S5.ForeignValueBind i c t
    definition' (S4.ValueBind i v) =
      scope $
        let
          (is, v1) = peelForAll4 v
          S4.TypedValue v2 t = copoint v1
        in
          case copoint v2 of
            S4.Function ps v3 -> do
              (v3', ds) <- value v3
              let
                S5.TypedValue _ t = copoint v3'
                v3'' =
                  case ds of
                    [] -> v3'
                    _  -> S5.TypedValue (S5.Let (ds <$ v3) v3' <$ v2) t <$ v
              pure $ S5.ValueBindN i is ps v3''
            S4.ForAllValue {} -> X.unreachable "for-all value even after peel"
            _ -> do
              (v1', ds) <- value v1
              let
                v1'' =
                  case ds of
                    [] -> v1'
                    _  -> S5.TypedValue (S5.Let (ds <$ v) v1' <$ v) t <$ v
              pure $ S5.ValueBindV i is v1''

dataRepresentation :: Traversable f => F f (S4.DataRepresentation et ev f) -> Pass (F f (S5.DataRepresentation et ev f))
dataRepresentation = traverse $ \(S4.ForAllData is cs) -> S5.ForAllData is <$> traverse (traverse valueConstructor) cs

valueConstructor :: Traversable f => F f (S4.ValueConstructor et ev f) -> Pass (F f (S5.ValueConstructor et ev f))
valueConstructor = traverse $ \(S4.ValueConstructor i fs) -> S5.ValueConstructor i <$> traverse (traverse field) fs

field :: Traversable f => F f (S4.Field et ev f) -> Pass (F f (S5.Field et ev f))
field = traverse $ \(S4.Field i t) -> pure $ S5.Field i t

value :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S4.Value et ev f) -> Pass (F f (S5.Value et ev f), [F f (S5.Definition et ev f)])
value v =
  case copoint v of
    S4.TypedValue v' t ->
      case copoint v' of
        S4.Variable i -> pure (S5.TypedValue (S5.Variable i <$ v') t <$ v, [])
        S4.Literal l -> do
          i <- (<$ v') <$> newIdentifier
          let m = S5.ValueBindV i ([] <$ v') (S5.TypedValue (S5.Literal l <$ v') t <$ v) <$ v'
          pure (S5.TypedValue (S5.Variable i <$ v') t <$ v', [m])
        S4.Function ps v'' -> do
          i <- (<$ v) <$> newIdentifier
          (v3, ds) <- value v''
          let (is, v4) = peelForAll5 v3
          let m = S5.ValueBindN i is ps v4 <$ v
          pure (S5.TypedValue (S5.Variable i <$ v') t <$ v, ds ++ [m])
        S4.Application v1 vs -> do
          (v1', ds) <- value v1
          (vs', dss) <- mapAndUnzipM value (copoint vs)
          pure (S5.TypedValue (S5.Application v1' (vs' <$ vs) <$ v') t <$ v, mconcat $ ds : dss)
        S4.Procedure ps -> do
          (ps', dss) <- N.unzip <$> mapM procedureStep (copoint ps)
          pure (S5.TypedValue (S5.Procedure (ps' <$ ps) <$ v') t <$ v, mconcat $ N.toList dss)
        S4.Let ds v1 -> do
          ds' <- mapM (traverse definition) ds
          (v1', vds) <- value v1
          pure (S5.TypedValue (S5.Let ds' v1' <$ v') t <$ v, vds)
        S4.ForAllValue i v1 -> do
          (v1', ds) <- value v1
          pure (S5.TypedValue (S5.ForAllValue i v1' <$ v') t <$ v, ds)
        S4.Instantiation v ts -> do
          (v', ds) <- value v
          pure (S5.TypedValue (S5.Instantiation v' ts <$ v) t <$ v, ds)

procedureStep :: (Traversable f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S4.ProcedureStep et ev f) -> Pass (F f (S5.ProcedureStep et ev f), [F f (S5.Definition et ev f)])
procedureStep s =
  case copoint s of
    S4.BindProcedureStep i v ->do
      (v', ds) <- value v
      pure (S5.BindProcedureStep i v' <$ v, ds)
    S4.CallProcedureStep v -> do
      (v', ds) <- value v
      pure (S5.CallProcedureStep v' <$ v, ds)

newIdentifier :: Pass SC.QualifiedIdentifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SC.LocalIdentifier $ SC.SystemIdentifier 'l' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r

peelForAll4 :: (Functor f, Copointed f) => F f (S4.Value et ev f) -> (F f [F f SC.QualifiedIdentifier], F f (S4.Value et ev f))
peelForAll4 v =
  case copoint v of
    S4.TypedValue v' _ ->
      case copoint v' of
        S4.ForAllValue i v'' ->
          let (is, v''') = peelForAll4 v''
          in ((i : copoint is) <$ v, v''')
        _ -> ([] <$ v, v)

peelForAll5 :: (Functor f, Copointed f) => F f (S5.Value et ev f) -> (F f [F f SC.QualifiedIdentifier], F f (S5.Value et ev f))
peelForAll5 v =
  case copoint v of
    S5.TypedValue v' _ ->
      case copoint v' of
        S5.ForAllValue i v'' ->
          let (is, v''') = peelForAll5 v''
          in ((i : copoint is) <$ v, v''')
        _ -> ([] <$ v, v)
