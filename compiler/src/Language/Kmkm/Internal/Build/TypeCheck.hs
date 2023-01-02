{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

#if __GLASGOW_HASKELL__ <= 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

-- | \"Type check\" pass.
module Language.Kmkm.Internal.Build.TypeCheck
  ( typeCheck
  , Exception (..)
  ) where

import           Language.Kmkm.Internal.Exception                                               (unreachable)
import qualified Language.Kmkm.Internal.Exception                                               as X
import qualified Language.Kmkm.Internal.Syntax.Core.Common                                      as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Curried                        as ST
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Curried.LambdaUnlifted   as S3
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Untyped.Curried.LambdaUnlifted as S2

import qualified Algebra.Graph.AdjacencyMap           as G hiding (vertexList)
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G hiding (topSort)
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Algebra.Graph.ToGraph                as G
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch, catchJust, throw)
import           Data.Copointed                       (Copointed (copoint))
import           Data.Either                          (fromRight)
import           Data.Functor                         ((<&>))
import qualified Data.Functor.Barbie.Layered          as B
import           Data.Functor.Classes                 (Eq1, Show1)
import           Data.Functor.F                       (F (F), unf)
import           Data.Functor.Identity                (Identity (Identity, runIdentity))
import           Data.Functor.With                    (MayHave)
import qualified Data.Functor.With                    as W
import           Data.List                            (foldl')
import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as N
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Typeable                        as Y
import           GHC.Generics                         (Generic)
import           GHC.Stack                            (HasCallStack)

type VariableTypes f = Map SC.QualifiedIdentifier (F f (ST.Type f))

typeCheck
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , B.FunctorB et
     , B.FunctorB ev
     , Eq1 f
     , Show1 f
     , HasCallStack
     )
  => Map SC.QualifiedIdentifier (F f (ST.Type f))
  -> F f (S2.Module et ev f)
  -> m (F f (S3.Module et ev f))
typeCheck variableTypes m =
  traverse go m
  where
    prim = primitivesImported m
    go (S2.Module mn ms ds) = S3.Module mn ms <$> definitions variableTypes prim ds

{-# ANN definitions ("HLint: ignore Use list literal" :: String) #-}

definitions
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , Eq1 f
     , Show1 f
     , HasCallStack
     )
  => VariableTypes f
  -> PrimitivesImported
  -> F f [F f (S2.Definition et ev f)]
  -> m (F f [F f (S3.Definition et ev f)])
definitions variableTypes prim =
  traverse go
  where
    go definitions' = do
      let
        valueBinds = M.fromList $ mapMaybe valueBind2 definitions'
        foreignValueBinds = M.fromList $ mapMaybe foreignValueBind definitions'
        dependencyGraph = G.overlays $ dependency (M.keysSet valueBinds) <$> definitions'
        sortedIdentifiers = fromRight (unreachable "empty") $ G.topSort $ G.scc dependencyGraph
        variableTypes' = M.unions $ M.mapMaybe annotatedType valueBinds : foreignValueBinds : variableTypes : [] -- why not a list literal? because of non-injective type families.
        typedValueBinds = M.unions $ mapMaybe valueConstructors definitions'
      typedValueBinds <- foldr (flip $ typeBind variableTypes' valueBinds prim) (pure typedValueBinds) sortedIdentifiers
      pure $ replaceValue typedValueBinds <$> definitions'

dependency :: (Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f (S2.Definition et ev f) -> G.AdjacencyMap SC.QualifiedIdentifier
dependency valueBinds b =
  case copoint b of
    S2.ValueBind i v -> G.vertex (copoint i) `G.connect` G.overlays (G.vertex <$> dep valueBinds v)
    _                -> G.empty

typeBind
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , Eq1 f
     , Show1 f
     )
  => VariableTypes f
  -> Map SC.QualifiedIdentifier (F f (S2.Value et ev f))
  -> PrimitivesImported
  -> m (Map SC.QualifiedIdentifier (F f (S3.Value et ev f)))
  -> GN.AdjacencyMap SC.QualifiedIdentifier
  -> m (Map SC.QualifiedIdentifier (F f (S3.Value et ev f)))
typeBind variableTypes valueBinds prim typedValueBinds recursionIdentifiers =
  catchJust
    (\case { NotFoundException i _ -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        variableTypes' = ((\v -> let S3.TypedValue _ t = copoint v in t) <$> typedValueBinds') `M.union` variableTypes
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- mapM (value variableTypes' prim) recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throw $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: Copointed f => F f (S2.Value et ev f) -> Maybe (F f (ST.Type f))
annotatedType v
  | S2.TypeAnnotation _ t <- copoint v = Just t
annotatedType _ = Nothing

replaceValue :: (Functor f, Copointed f, HasCallStack) => Map SC.QualifiedIdentifier (F f (S3.Value et ev f)) -> F f (S2.Definition et ev f) -> F f (S3.Definition et ev f)
replaceValue typedValueBinds =
  fmap $ \case
    S2.DataDefinition i r ->
      let
        dataRepresentation r =
          r <&> \case
            S2.ForAllData i r           -> S3.ForAllData i $ dataRepresentation r
            S2.ValueConstructorsData cs -> S3.ValueConstructorsData $ (valueConstructor <$>) <$> cs
        valueConstructor c = (\(S2.ValueConstructor i fs) -> S3.ValueConstructor i $ (field <$>) <$> fs) <$> c
        field f = (\(S2.Field i t) -> S3.Field i t) <$> f
      in S3.DataDefinition i $ dataRepresentation r
    S2.TypeBind i t -> S3.TypeBind i t
    S2.ForeignTypeBind i c -> S3.ForeignTypeBind i c
    S2.ValueBind i _ -> S3.ValueBind i $ fromMaybe (unreachable "not found") $ M.lookup (copoint i) typedValueBinds
    S2.ForeignValueBind i c t -> S3.ForeignValueBind i c t

valueBind2 :: Copointed f => F f (S2.Definition et ev f) -> Maybe (SC.QualifiedIdentifier, F f (S2.Value et ev f))
valueBind2 d =
  case copoint d of
    S2.ValueBind i v -> Just (copoint i, v)
    _                -> Nothing

valueBind3 :: Copointed f => F f (S3.Definition et ev f) -> Maybe (SC.QualifiedIdentifier, F f (S3.Value et ev f))
valueBind3 d =
  case copoint d of
    S3.ValueBind i v -> Just (copoint i, v)
    _                -> Nothing

foreignValueBind :: Copointed f => F f (S2.Definition et ev f) -> Maybe (SC.QualifiedIdentifier, F f (ST.Type f))
foreignValueBind d | S2.ForeignValueBind i _ t <- copoint d       = Just (copoint i, t)
foreignValueBind _                                               = Nothing

valueConstructors :: (Functor f, Copointed f) => F f (S2.Definition et ev f) -> Maybe (Map SC.QualifiedIdentifier (F f (S3.Value et ev f)))
valueConstructors d
  | S2.DataDefinition i r <- copoint d =
      let
        dataRepresentation r =
          case copoint r of
            S2.ValueConstructorsData cs ->
              M.fromList $ flip fmap (copoint cs) $ \c ->
                let
                  S2.ValueConstructor j fs = copoint c
                  go f r =
                    let S2.Field _ t = copoint f
                    in S3.FunctionType t r <$ f
                in (copoint j, S3.TypedValue (S3.Variable j <$ j) (foldr go (S3.TypeVariable i <$ i) $ copoint fs) <$ r)
            S2.ForAllData a r' -> fmap (\(S3.TypedValue v t) -> S3.TypedValue v $ S3.ForAllType a t <$ r) <$> dataRepresentation r'
      in Just $ dataRepresentation r
valueConstructors _ = Nothing

dep :: (Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f (S2.Value et ev f) -> [SC.QualifiedIdentifier]
dep identifiers v =
  case copoint v of
        S2.Variable i ->
          let i' = copoint i
          in [i' | i' `S.member` identifiers]
        S2.Function i _ v' -> dep (S.delete (copoint i) identifiers) v'
        S2.Literal _ -> []
        S2.Application v1 v2 -> mconcat $ dep identifiers <$> [v1, v2]
        S2.Procedure ps ->
          fst $ foldl' go ([], identifiers) $ copoint ps
          where
            go (is, ss) s =
              case copoint s of
                S2.BindProcedureStep i v' -> (dep ss v' ++ is, S.delete (copoint i) ss)
                S2.CallProcedureStep v'   -> (dep ss v' ++ is, ss)
        S2.TypeAnnotation v' _ -> dep identifiers v'
        S2.Let ds v ->
          let
            valueBinds = M.fromList $ mapMaybe valueBind2 $ copoint ds
            identifiers' = identifiers S.\\ M.keysSet valueBinds
          in dep identifiers' =<< v : M.elems valueBinds
        S2.ForAllValue _ v -> dep identifiers v
        S2.Instantiation v _ -> dep identifiers v

value
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , Eq1 f
     , Show1 f
     )
  => VariableTypes f
  -> PrimitivesImported
  -> F f (S2.Value et ev f)
  -> m (F f (S3.Value et ev f))
value variableTypes prim@PrimitivesImported { int = primInt, frac2 = primFrac2, string = primString } v = do
  case copoint v of
    S2.Variable i ->
      let i' = copoint i
      in
        case M.lookup i' variableTypes of
          Nothing -> throw $ NotFoundException i' $ W.mayGet i
          Just t  -> pure $ S3.TypedValue (S3.Variable i <$ v) t <$ v
    S2.Literal l ->
      case copoint l of
        S2.Integer v_ b ->
          let int = SC.GlobalIdentifier ["kmkm", "prim"] "int"
          in
            if primInt
              then pure $ S3.TypedValue (S3.Literal (S3.Integer v_ b <$ l) <$ v) (S3.TypeVariable (int <$ v) <$ v) <$ v
              else throw $ PrimitiveTypeException int $ W.mayGet v
        S2.Fraction s d e b ->
          let frac2 = SC.GlobalIdentifier ["kmkm", "prim"] "frac2"
          in
            if primFrac2
              then pure $ S3.TypedValue (S3.Literal (S3.Fraction s d e b <$ l) <$ v) (S3.TypeVariable (frac2 <$ v) <$ v) <$ v
              else throw $ PrimitiveTypeException frac2 $ W.mayGet v
        S2.String t ->
          let string = SC.GlobalIdentifier ["kmkm", "prim"] "string"
          in
            if primString
              then pure $ S3.TypedValue (S3.Literal (S3.String t <$ l) <$ v) (S3.TypeVariable (string <$ v) <$ v) <$ v
              else throw $ PrimitiveTypeException string $ W.mayGet v
    S2.Function i t v'' -> do
          v''' <- value (M.insert (copoint i) t variableTypes) prim v''
          let S3.TypedValue _ t' = copoint v'''
          pure $ S3.TypedValue (S3.Function i t v''' <$ v) (S3.FunctionType t t' <$ v) <$ v
    S2.Application v0 v1 -> do
          v0' <- value variableTypes prim v0
          v1' <- value variableTypes prim v1
          let
            S3.TypedValue _ (F t0) = copoint v0'
            S3.TypedValue _ (F t1) = copoint v1'
          case copoint t0 of
            S3.FunctionType (F t00) t01
              | SC.toIdentity t1 == SC.toIdentity t00 -> pure $ S3.TypedValue (S3.Application v0' v1' <$ v) t01 <$ v
              | otherwise                           -> throw $ MismatchException (Right $ SC.toIdentity t00) (SC.toIdentity t1) $ W.mayGet v1'
            _ -> throw $ MismatchException (Left "function") (SC.toIdentity t0) $ W.mayGet v0'
    S2.Procedure ps -> do
      let p :| ps' = copoint ps
      (variableTypes', p') <- procedureStep variableTypes prim p
      (_, ps'') <- foldr go (pure (variableTypes', [])) ps'
      let ps''' = p' :| ps''
      case N.last ps''' of
        s
          | S3.CallProcedureStep v <- copoint s ->
              let S3.TypedValue _ t = copoint v
              in pure $ S3.TypedValue (S3.Procedure (ps''' <$ ps) <$ v) t <$ v
          | otherwise -> throw $ BindProcedureEndException $ W.mayGet s
      where
        go p acc = do
          (variableTypes, ps) <- acc
          (variableTypes', p') <- procedureStep variableTypes prim p
          pure (variableTypes', p' : ps)
    S2.TypeAnnotation v' (F t) -> do
      v'' <- value variableTypes prim v'
      let S3.TypedValue _ (F t') = copoint v''
      if SC.toIdentity t == SC.toIdentity t'
        then pure v''
        else throw $ MismatchException (Right $ SC.toIdentity t) (SC.toIdentity t') $ W.mayGet v''
    S2.Let ds v' -> do
      ds' <- definitions variableTypes prim ds
      let variableTypes' = ((\(S3.TypedValue _ t) -> t) . copoint <$> M.fromList (mapMaybe valueBind3 $ copoint ds')) `M.union` variableTypes
      v'' <- value variableTypes' prim v'
      let S3.TypedValue _ t' = copoint v''
      pure $ S3.TypedValue (S3.Let ds' v'' <$ v') t' <$ v
    S2.ForAllValue i v' -> do
      let variableTypes' = M.insert (copoint i) (S3.TypeVariable i <$ i) variableTypes
      v'' <- value variableTypes' prim v'
      let S3.TypedValue _ t = copoint v''
      pure $ S3.TypedValue (S3.ForAllValue i v'' <$ v) (S3.ForAllType i t <$ v) <$ v
    S2.Instantiation v0 t1 -> do
      v0' <- value variableTypes prim v0
      let S3.TypedValue _ (F t0) = copoint v0'
      case copoint t0 of
        S3.ForAllType j t01 -> do
          let t01' = substitute j t1 t01
          pure $ S3.TypedValue (S3.Instantiation v0' t1 <$ v) t01' <$ v
        _ -> throw $ MismatchException (Left "for-all value") (SC.toIdentity t0) $ W.mayGet v0'

substitute :: (Functor f, Copointed f) => F f SC.QualifiedIdentifier -> F f (ST.Type f) -> F f (ST.Type f) -> F f (ST.Type f)
substitute i value target =
  case copoint target of
    ST.TypeVariable i'
      | copoint i == copoint i' -> value
      | otherwise -> target
    ST.TypeApplication t0 t1 -> ST.TypeApplication (substitute i value t0) (substitute i value t1) <$ target
    ST.FunctionType t0 t1 -> ST.FunctionType (substitute i value t0) (substitute i value t1) <$ target
    ST.ProcedureType t0 -> ST.ProcedureType (substitute i value t0) <$ target
    ST.ForAllType i' t
      | copoint i == copoint i' -> target
      | otherwise -> ST.ForAllType i' (substitute i value t) <$ target

procedureStep
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , Eq1 f
     , Show1 f
     )
  => VariableTypes f
  -> PrimitivesImported
  -> F f (S2.ProcedureStep et ev f)
  -> m (VariableTypes f, F f (S3.ProcedureStep et ev f))
procedureStep variableTypes prim s =
  case copoint s of
    S2.BindProcedureStep i v -> do
      v' <- value variableTypes prim v
      let
        S3.TypedValue _ t = copoint v'
        variableTypes' = M.insert (copoint i) t variableTypes
      pure (variableTypes', S3.BindProcedureStep i v' <$ s)
    S2.CallProcedureStep v -> do
      v' <- value variableTypes prim v
      let S3.TypedValue _ (F t) = copoint v'
      case copoint t of
        ST.ProcedureType _ -> pure (variableTypes, S3.CallProcedureStep v' <$ s)
        _                  -> throw $ MismatchException (Left "procedure") (SC.toIdentity t) $ W.mayGet t

primitivesImported :: (Functor f, Copointed f, B.FunctorB et, B.FunctorB ev) => F f (S2.Module et ev f) -> PrimitivesImported
primitivesImported (F m) =
  case SC.toIdentity m of
    Identity (S2.Module _ (F ms) _) ->
      if Identity ["kmkm", "prim"] `elem` (unf <$> runIdentity ms)
        then PrimitivesImported { int = True, uint = True, frac2 = True, string = True }
        else PrimitivesImported { int = False, uint = False, frac2 = False, string = False }

data PrimitivesImported =
  PrimitivesImported
    { int    :: Bool
    , uint   :: Bool
    , frac2  :: Bool
    , string :: Bool
    }
  deriving (Show, Read, Eq, Ord, Generic)

data Exception
  = NotFoundException SC.QualifiedIdentifier (Maybe SC.Location)
  | MismatchException { expected :: Either Text (Identity (ST.Type Identity)), actual :: Identity (ST.Type Identity), location :: Maybe SC.Location }
  | BindProcedureEndException (Maybe SC.Location)
  | RecursionException (Set SC.QualifiedIdentifier)
  | PrimitiveTypeException SC.QualifiedIdentifier (Maybe SC.Location)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
