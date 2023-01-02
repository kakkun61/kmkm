{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.Kmkm.Internal.Build.C.PolymorphicPoint
  ( attach
  , variablePolymorphicPoints
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Exception                                             as X
import qualified Language.Kmkm.Internal.Syntax.C.PolymorphicPointed                           as S6
import qualified Language.Kmkm.Internal.Syntax.Core.Common                                    as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaLifted as S5

import qualified Control.Exception           as E
import           Control.Exception.Safe      (MonadThrow, throw)
import           Control.Monad.Identity      (Identity (Identity))
import           Data.Bool                   (bool)
import           Data.Copointed              (Copointed (copoint))
import           Data.Foldable               (Foldable (fold))
import           Data.Functor                ((<&>))
import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.F              (F (F), unf)
import           Data.Functor.Identity       (Identity (runIdentity))
import           Data.Functor.With           (MayHave)
import qualified Data.Functor.With           as W
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import qualified Data.Typeable               as Y
import           GHC.Generics                (Generic)
import           GHC.Stack                   (HasCallStack)

type VariablePolymorphicPoints = Map S5.ReferenceIdentifier S6.PolymorphicPoint

attach
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , HasCallStack
     )
  => Map S5.ReferenceIdentifier S6.PolymorphicPoint
  -> F f (S5.Module SC.EmbeddedCType SC.EmbeddedCValue f)
  -> m (F f (S6.Module SC.EmbeddedCType SC.EmbeddedCValue f))
attach = module'

module'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , HasCallStack
     )
  => VariablePolymorphicPoints
  -> F f (S5.Module SC.EmbeddedCType SC.EmbeddedCValue f)
  -> m (F f (S6.Module SC.EmbeddedCType SC.EmbeddedCValue f))
module' vpps =
  traverse $ \(S5.Module n is ds) -> do
    ds' <- traverse (traverse (valueDefinition vpps)) ds
    pure $ S6.Module n is ds'

valueDefinition
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , HasCallStack
     )
  => VariablePolymorphicPoints
  -> F f (S5.Definition SC.EmbeddedCType SC.EmbeddedCValue f)
  -> m (F f (S6.Definition SC.EmbeddedCType SC.EmbeddedCValue f))
valueDefinition vpps =
  traverse $ \case
    (S5.DataDefinition i r)     -> pure $ S6.DataDefinition i $ dataRepresentation r
    (S5.ValueBindV i ts v)      -> S6.ValueBindV i ts <$> value vpps v
    (S5.ValueBindN i ts is v)   -> S6.ValueBindN i ts is <$> value vpps v -- TODO i（i は is に含まれる）を vpps に追加する
    (S5.TypeBind i t)           -> pure $ S6.TypeBind i t
    (S5.ForeignTypeBind i t)    -> pure $ S6.ForeignTypeBind i t
    (S5.ForeignValueBind i v t) -> pure $ S6.ForeignValueBind i v t

dataRepresentation :: Functor f => F f (S5.DataRepresentation et ev f) -> F f (S6.DataRepresentation et ev f)
dataRepresentation = fmap $ \(S5.ForAllData ts cs) -> S6.ForAllData ts (fmap valueConstructor <$> cs)

valueConstructor :: Functor f => F f (S5.ValueConstructor et ev f) -> F f (S6.ValueConstructor et ev f)
valueConstructor = fmap $ \(S5.ValueConstructor is fs) -> S6.ValueConstructor is (fmap field <$> fs)

field :: Functor f => F f (S5.Field et ev f) -> F f (S6.Field et ev f)
field = fmap $ \(S5.Field i t) -> S6.Field i t

value
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , HasCallStack
     )
  => VariablePolymorphicPoints
  -> F f (S5.Value et ev f)
  -> m (F f (S6.Value et ev f))
value vpps v = do
  let
    (S5.TypedValue v' t) = copoint v
  case copoint v' of
    S5.Variable n -> do
      let n' = copoint n
      case M.lookup n' vpps of
        Nothing  -> throw $ NotFoundException n' $ W.mayGet n
        Just vpp -> pure $ S6.TypedValue (S6.Variable n <$ v') t vpp <$ v
    S5.Literal l -> pure $ S6.TypedValue (S6.Literal l <$ v') t (S6.PolymorphicPoint S6.Monoparam []) <$ v
    S5.Function {} -> undefined -- TODO ここ来える？
    S5.Application v1 v2s -> do
      v1_ <- value vpps v1
      let S6.TypedValue _ _ (S6.PolymorphicPoint pp _) = copoint v1_
      v2s_ <- traverse (traverse $ value vpps) v2s
      pure $ S6.TypedValue (S6.Application v1_ v2s_ <$ v') t (S6.PolymorphicPoint pp []) <$ v
    S5.Procedure ps -> undefined
    S5.Let ds v'' -> undefined
    S5.ForAllValue i v'' -> undefined
    S5.Instantiation v'' t -> undefined

variablePolymorphicPoints :: (Functor f, Copointed f) => F f (S5.Module SC.EmbeddedCType SC.EmbeddedCValue f) -> Map S5.ReferenceIdentifier S6.PolymorphicPoint
variablePolymorphicPoints m =
  let (S5.Module _ _ (F (Identity ds))) = bmap (Identity . copoint) $ copoint m
  in
    fold $ flip mapMaybe ds $ \case
      F (Identity (S5.ValueBindV (F (Identity i)) (F (Identity ts)) (F (Identity (S5.TypedValue _ t))))) ->
        let ts' = S.fromList $ copoint <$> ts
        in Just $ M.singleton i $ S6.PolymorphicPoint (isPolymorphic ts' t) []
      F (Identity (S5.ValueBindN (F (Identity i)) (F (Identity ts)) (F (Identity ps)) (F (Identity (S5.TypedValue _ t))))) ->
        let
          ts' = S.fromList $ copoint <$> ts
          ps' = snd . runIdentity . unf <$> ps
        in Just $ M.singleton i $ S6.PolymorphicPoint (isPolymorphic ts' t) $ isPolymorphic ts' <$> ps'
      F (Identity (S5.DataDefinition _ r)) -> Just $ dataRepresentation r
      _                               -> Nothing
  where
    dataRepresentation (F (Identity (S5.ForAllData (F (Identity ts)) (F (Identity cs))))) =
      let
        ts' = S.fromList $ copoint <$> ts
      in
        M.fromList $
          cs <&> \c ->
            let (i, ps) = valueConstructor ts' c
            in (i, S6.PolymorphicPoint S6.Polyparam ps)

    valueConstructor ts (F (Identity (S5.ValueConstructor (F (Identity i)) (F (Identity fs))))) = (i, isPolymorphicField ts <$> fs)

    isPolymorphicField ts (F (Identity (S5.Field _ t))) = isPolymorphic ts t

isPolymorphic :: Set S5.ReferenceIdentifier -> F Identity (S5.Type Identity) -> S6.ParameterPolymorphic
isPolymorphic ts (F (Identity t)) =
  case t of
    (S5.TypeVariable (F (Identity i))) -> bool S6.Polyparam S6.Monoparam $ S.member i ts
    (S5.TypeApplication t1 t2) -> case (isPolymorphic ts t1, isPolymorphic ts t2) of { (S6.Monoparam, S6.Monoparam) -> S6.Monoparam; _ -> S6.Polyparam }
    (S5.FunctionType _ _) -> S6.Monoparam -- TODO これもポインター？
    (S5.ProcedureType t) -> isPolymorphic ts t
    (S5.ForAllType (F (Identity i)) t) -> isPolymorphic (S.insert i ts) t

data Exception
  = NotFoundException S5.ReferenceIdentifier (Maybe SC.Location)
  | NoMetadataException (Maybe SC.Location)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
