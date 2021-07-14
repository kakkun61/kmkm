{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors      #-}
#else
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

-- | \"Type check\" pass.
module Language.Kmkm.Build.TypeCheck
  ( typeCheck
  , Exception (..)
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Exception as X
import qualified Language.Kmkm.Syntax    as S

import qualified Algebra.Graph.AdjacencyMap           as G hiding (vertexList)
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G hiding (topSort)
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Algebra.Graph.ToGraph                as G
import qualified Barbies.Bare                         as B
import           Barbies.Bare.Layered                 (BareB, bstripFrom)
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch)
import           Control.Monad.Catch                  (MonadThrow (throwM), catchJust)
import           Data.Copointed                       (Copointed (copoint))
import           Data.Either                          (fromRight)
import           Data.Functor.Identity                (Identity)
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

type Module t f = S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t B.Covered f

type Definition t f = S.Definition 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t B.Covered f

type Type f = S.Type 'S.NameResolved 'S.Curried B.Covered f

type Value t f = S.Value 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t B.Covered f

type ProcedureStep t f = S.ProcedureStep 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t B.Covered f

typeCheck
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     , Eq (f (S.Type 'S.NameResolved 'S.Curried B.Covered f))
     , Show (f (S.Type 'S.NameResolved 'S.Curried B.Covered f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (S.Type 'S.NameResolved 'S.Curried B.Covered f))
  -> f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed B.Covered f))
typeCheck = typeCheck'

typeCheck'
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier))
  => Map S.QualifiedIdentifier (f (Type f))
  -> f (Module 'S.Untyped f)
  -> m (f (Module 'S.Typed f))
typeCheck' ctx =
  traverse go
  where
    go (S.Module mn ms ds) = S.Module mn ms <$> definitions ctx ds

definitions
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> f [f (Definition 'S.Untyped f)]
  -> m (f [f (Definition 'S.Typed f)])
definitions context =
  traverse go
  where
    -- go :: [f (Definition 'S.Untyped f)] -> m [f (Definition 'S.Typed f)]
    go definitions' = do
      let
        valueBinds = M.fromList $ mapMaybe valueBind definitions'
        dependencyGraph = G.overlays $ dependency (M.keysSet valueBinds) <$> definitions'
        sortedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencyGraph
        context' = M.mapMaybe annotatedType valueBinds `M.union` context
      typedValueBinds <- foldr (typeBind context' valueBinds) (pure M.empty) sortedIdentifiers
      pure $ replaceTerm typedValueBinds <$> definitions'

dependency :: (Copointed f, S.HasPosition f) => Set S.QualifiedIdentifier -> f (Definition 'S.Untyped f) -> G.AdjacencyMap S.QualifiedIdentifier
dependency valueBinds d | S.ValueBind (S.ValueBindU i v) <- copoint d = G.vertex (copoint i) `G.connect` G.overlays (G.vertex <$> dep valueBinds v)
dependency _ _                                                        = G.empty

typeBind
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> Map S.QualifiedIdentifier (f (Value 'S.Untyped f))
  -> GN.AdjacencyMap S.QualifiedIdentifier
  -> m (Map S.QualifiedIdentifier (f (Value 'S.Typed f)))
  -> m (Map S.QualifiedIdentifier (f (Value 'S.Typed f)))
typeBind context valueBinds recursionIdentifiers typedValueBinds =
  catchJust
    (\case { NotFoundException i _ -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        context' = ((\v -> let S.TypedValue _ t = copoint v in t) <$> typedValueBinds') `M.union` context
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- sequence $ typeOfTerm context' <$> recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throwM $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: Copointed f => f (Value 'S.Untyped f) -> Maybe (f (Type f))
annotatedType v
  | S.UntypedValue v <- copoint v
  , S.TypeAnnotation (S.TypeAnnotation' _ t) <- copoint v = Just t
annotatedType _                                                           = Nothing

replaceTerm :: (Functor f, Copointed f) => Map S.QualifiedIdentifier (f (Value 'S.Typed f)) -> f (Definition 'S.Untyped f) -> f (Definition 'S.Typed f)
replaceTerm typedValueBinds =
  fmap $ \case
    S.DataDefinition i cs -> S.DataDefinition i cs
    S.TypeBind i t -> S.TypeBind i t
    S.ForeignTypeBind i hs c -> S.ForeignTypeBind i hs c
    S.ValueBind (S.ValueBindU i _) ->
      S.ValueBind (S.ValueBindU i $ fromMaybe unreachable $ M.lookup (copoint i) typedValueBinds)
    S.ForeignValueBind i hs c t -> S.ForeignValueBind i hs c t

valueBind :: Copointed f => f (Definition t f) -> Maybe (S.QualifiedIdentifier, f (Value t f))
valueBind d | S.ValueBind (S.ValueBindU i v) <- copoint d = Just (copoint i, v)
valueBind _                                = Nothing

dep :: (Copointed f, S.HasPosition f) => Set S.QualifiedIdentifier -> f (Value 'S.Untyped f) -> [S.QualifiedIdentifier]
dep identifiers v =
  case copoint v of
    S.UntypedValue v' ->
      case copoint v' of
        S.Variable i
          | i' <- copoint i
          , i' `S.member` identifiers -> [i']
          | otherwise -> []
        S.Function (S.FunctionC i _ v') -> dep (S.delete (copoint i) identifiers) v'
        S.Literal _ -> []
        S.Application (S.ApplicationC v1 v2) -> mconcat $ dep identifiers <$> [v1, v2]
        S.Procedure ps ->
          fst $ foldl' go ([], identifiers) $ copoint ps
          where
            go (is, ss) s =
              case copoint s of
                S.BindProcedure i v' -> (dep ss v' ++ is, S.delete (copoint i) ss)
                S.TermProcedure v'   -> (dep ss v' ++ is, ss)
        S.TypeAnnotation (S.TypeAnnotation' v' _) -> dep identifiers v'
        S.Let ds v ->
          let
            valueBinds = M.fromList $ mapMaybe valueBind $ copoint ds
            identifiers' = identifiers S.\\ M.keysSet valueBinds
          in dep identifiers' =<< v : M.elems valueBinds

typeOfTerm
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> f (Value 'S.Untyped f)
  -> m (f (Value 'S.Typed f))
typeOfTerm ctx v =
  case copoint v of
    S.UntypedValue v' ->
      case copoint v' of
        S.Variable i ->
          let i' = copoint i
          in
            case M.lookup i' ctx of
              Nothing -> throwM $ NotFoundException i' $ S.range i
              Just t  -> pure $ S.TypedValue (S.Variable i <$ v') t <$ v
        S.Literal (S.Integer v_ b) ->
          pure $ S.TypedValue (S.Literal (S.Integer v_ b) <$ v') (S.TypeVariable (S.GlobalIdentifier ["kmkm", "prim"] "int" <$ v) <$ v) <$ v
        S.Literal (S.Fraction s d e b) ->
          pure $ S.TypedValue (S.Literal (S.Fraction s d e b) <$ v') (S.TypeVariable (S.GlobalIdentifier ["kmkm", "prim"] "frac2" <$ v) <$ v) <$ v
        S.Literal (S.String t) ->
          pure $ S.TypedValue (S.Literal (S.String t) <$ v') (S.TypeVariable (S.GlobalIdentifier ["kmkm", "prim"] "string" <$ v) <$ v) <$ v
        S.Function (S.FunctionC i t v'') -> do
          v''' <- typeOfTerm (M.insert (copoint i) t ctx) v''
          let S.TypedValue _ t' = copoint v'''
          pure $ S.TypedValue (S.Function (S.FunctionC i t v''') <$ v') (S.FunctionType (S.FunctionTypeC t t') <$ v) <$ v
        S.Application (S.ApplicationC v0 v1) -> do
          v0' <- typeOfTerm ctx v0
          v1' <- typeOfTerm ctx v1
          let
            S.TypedValue _ t0 = copoint v0'
            S.TypedValue _ t1 = copoint v1'
          case copoint t0 of
            S.FunctionType (S.FunctionTypeC t00 t01)
              | strip t1 == strip t00 -> pure $ S.TypedValue (S.Application (S.ApplicationC v0' v1') <$ v') t01 <$ v
              | otherwise -> throwM $ MismatchException (Right $ strip t00) (strip t1) $ S.range t1
            _ -> throwM $ MismatchException (Left "function") (strip t0) $ S.range t0
        S.Procedure ps -> do
          let p :| ps' = copoint ps
          (ctx', p') <- typeOfProcedure ctx p
          (_, ps'') <- foldr go (pure (ctx', [])) ps'
          let ps''' = p' :| ps''
          case N.last ps''' of
            s
              | S.TermProcedure v <- copoint s ->
                  let S.TypedValue _ t = copoint v
                  in pure $ S.TypedValue (S.Procedure (ps''' <$ ps) <$ v') t <$ v
              | otherwise -> throwM $ BindProcedureEndException $ S.range s
          where
            go p acc = do
              (ctx, ps) <- acc
              (ctx', p') <- typeOfProcedure ctx p
              pure (ctx', p' : ps)
        S.TypeAnnotation (S.TypeAnnotation' v' t) -> do
          v'' <- typeOfTerm ctx v'
          let S.TypedValue _ t' = copoint v''
          if strip t == strip t'
            then pure v''
            else throwM $ MismatchException (Right $ strip t) (strip t') $ S.range t'
        S.Let ds v' -> do
          ds' <- definitions ctx ds
          let ctx' = ((\(S.TypedValue _ t) -> t) . copoint <$> M.fromList (mapMaybe valueBind $ copoint ds')) `M.union` ctx
          v'' <- typeOfTerm ctx' v'
          let S.TypedValue _ t' = copoint v''
          pure $ S.TypedValue (S.Let ds' v'' <$ v') t' <$ v

typeOfProcedure
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
  )
  => Map S.QualifiedIdentifier (f (Type f))
  -> f (ProcedureStep 'S.Untyped f)
  -> m (Map S.QualifiedIdentifier (f (Type f)), f (ProcedureStep 'S.Typed f))
typeOfProcedure ctx s =
  case copoint s of
    S.BindProcedure i v -> do
      v' <- typeOfTerm ctx v
      let
        S.TypedValue _ t = copoint v'
        ctx' = M.insert (copoint i) t ctx
      pure (ctx', S.BindProcedure i v' <$ v)
    S.TermProcedure v -> do
      v' <- typeOfTerm ctx v
      pure (ctx, S.TermProcedure v' <$ v)

strip :: (Functor f, Copointed f, BareB b) => f (b B.Covered f) -> b B.Bare Identity
strip = bstripFrom copoint . copoint

data Exception
  = NotFoundException S.QualifiedIdentifier (Maybe (S.Position, S.Position))
  | MismatchException { expected :: Either Text (S.Type 'S.NameResolved 'S.Curried B.Bare Identity), actual :: S.Type 'S.NameResolved 'S.Curried B.Bare Identity, range :: Maybe (S.Position, S.Position) }
  | BindProcedureEndException (Maybe (S.Position, S.Position))
  | RecursionException (Set S.QualifiedIdentifier)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
