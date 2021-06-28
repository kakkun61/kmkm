{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , Module
  , Type
  , Exception (..)
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Exception as X
import qualified Language.Kmkm.Syntax    as S

import qualified Algebra.Graph.AdjacencyMap           as G hiding (vertexList)
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G hiding (topSort)
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Algebra.Graph.ToGraph                as G
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch)
import           Control.Monad.Catch                  (MonadThrow (throwM), catchJust)
import           Data.Either                          (fromRight)
import           Data.List                            (foldl')
import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as N
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import qualified Data.Typeable                        as Y
import           GHC.Generics                         (Generic)

type Module t = S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t

type Definition t = S.Definition 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t

type Type = S.Type 'S.NameResolved 'S.Curried

type Value t = S.Value 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t

type ProcedureStep t = S.ProcedureStep 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t

typeCheck :: (MonadThrow m, MonadCatch m) => Map S.QualifiedIdentifier Type -> Module 'S.Untyped -> m (Module 'S.Typed)
typeCheck ctx (S.Module mn ds ms) = S.Module mn ds <$> definitions ctx ms

definitions :: (MonadThrow m, MonadCatch m) => Map S.QualifiedIdentifier Type -> [Definition 'S.Untyped] -> m [Definition 'S.Typed]
definitions context definitions' = do
  let
    valueBinds = M.fromList $ mapMaybe valueBind definitions'
    dependencyGraph = G.overlays $ dependency (M.keysSet valueBinds) <$> definitions'
    sortedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencyGraph
    context' = M.mapMaybe annotatedType valueBinds `M.union` context
  typedValueBinds <- foldr (typeBind context' valueBinds) (pure M.empty) sortedIdentifiers
  pure $ replaceTerm typedValueBinds <$> definitions'

dependency :: Set S.QualifiedIdentifier -> Definition 'S.Untyped -> G.AdjacencyMap S.QualifiedIdentifier
dependency valueBinds (S.ValueBind (S.ValueBindU i v)) = G.vertex i `G.connect` G.overlays (G.vertex <$> dep valueBinds v)
dependency _ _                                    = G.empty

typeBind
  :: (MonadThrow m, MonadCatch m)
  => Map S.QualifiedIdentifier Type
  -> Map S.QualifiedIdentifier (Value 'S.Untyped)
  -> GN.AdjacencyMap S.QualifiedIdentifier
  -> m (Map S.QualifiedIdentifier (Value 'S.Typed))
  -> m (Map S.QualifiedIdentifier (Value 'S.Typed))
typeBind context valueBinds recursionIdentifiers typedValueBinds =
  catchJust
    (\e -> case e of { NotFoundException i -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        context' = ((\(S.TypedTerm _ t) -> t) <$> typedValueBinds') `M.union` context
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- sequence $ typeOfTerm context' <$> recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throwM $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: Value 'S.Untyped -> Maybe Type
annotatedType (S.UntypedValue (S.TypeAnnotation (S.TypeAnnotation' _ t))) = Just t
annotatedType _                                                           = Nothing

replaceTerm :: Map S.QualifiedIdentifier (Value 'S.Typed) -> Definition 'S.Untyped -> Definition 'S.Typed
replaceTerm _ (S.DataDefinition i cs) = S.DataDefinition i cs
replaceTerm _ (S.TypeBind i t) = S.TypeBind i t
replaceTerm typedValueBinds (S.ValueBind (S.ValueBindU i _)) =
  S.ValueBind (S.ValueBindU i $ fromMaybe unreachable $ M.lookup i typedValueBinds)
replaceTerm _ (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c t

valueBind :: Definition t -> Maybe (S.QualifiedIdentifier, Value t)
valueBind (S.ValueBind (S.ValueBindU i v)) = Just (i, v)
valueBind _                                = Nothing

dep :: Set S.QualifiedIdentifier -> Value 'S.Untyped -> [S.QualifiedIdentifier]
dep identifiers (S.UntypedValue (S.Variable i))
  | i `S.member` identifiers = [i]
  | otherwise                = []
dep identifiers (S.UntypedValue (S.Literal (S.Function (S.FunctionC i _ v)))) = dep (S.delete i identifiers) v
dep _ (S.UntypedValue (S.Literal _)) = []
dep identifiers (S.UntypedValue (S.Application (S.ApplicationC v1 v2))) = mconcat $ dep identifiers <$> [v1, v2]
dep identifiers (S.UntypedValue (S.Procedure ps)) =
  fst $ foldl' go ([], identifiers) ps
  where
    go (is, ss) (S.BindProcedure i v) = (dep ss v ++ is, S.delete i ss)
    go (is, ss) (S.TermProcedure v)   = (dep ss v ++ is, ss)
dep identifiers (S.UntypedValue (S.TypeAnnotation (S.TypeAnnotation' v _))) = dep identifiers v
dep identifiers (S.UntypedValue (S.Let ds v)) =
  let
    valueBinds = M.fromList $ mapMaybe valueBind ds
    identifiers' = identifiers S.\\ M.keysSet valueBinds
  in dep identifiers' =<< v : M.elems valueBinds

typeOfTerm :: (MonadThrow m, MonadCatch m) => Map S.QualifiedIdentifier Type -> Value 'S.Untyped -> m (Value 'S.Typed)
typeOfTerm ctx (S.UntypedValue (S.Variable i)) =
  case M.lookup i ctx of
    Nothing -> throwM $ NotFoundException i
    Just t  -> pure $ S.TypedTerm (S.Variable i) t
typeOfTerm _ (S.UntypedValue (S.Literal (S.Integer v b))) = pure $ S.TypedTerm (S.Literal (S.Integer v b)) (S.TypeVariable (S.GlobalIdentifier "kmkm.prim" "int"))
typeOfTerm _ (S.UntypedValue (S.Literal (S.Fraction s d e b))) = pure $ S.TypedTerm (S.Literal (S.Fraction s d e b)) (S.TypeVariable (S.GlobalIdentifier "kmkm.prim" "frac2"))
typeOfTerm _ (S.UntypedValue (S.Literal (S.String t))) = pure $ S.TypedTerm (S.Literal (S.String t)) (S.TypeVariable (S.GlobalIdentifier "kmkm.prim" "string"))
typeOfTerm ctx (S.UntypedValue (S.Literal (S.Function (S.FunctionC i t v)))) = do
  v'@(S.TypedTerm _ t') <- typeOfTerm (M.insert i t ctx) v
  pure $ S.TypedTerm (S.Literal (S.Function (S.FunctionC i t v'))) (S.FunctionType $ S.FunctionTypeC t t')
typeOfTerm ctx (S.UntypedValue (S.Application (S.ApplicationC v0 v1))) = do
  v0'@(S.TypedTerm _ t0) <- typeOfTerm ctx v0
  v1'@(S.TypedTerm _ t1) <- typeOfTerm ctx v1
  case t0 of
    S.FunctionType (S.FunctionTypeC t00 t01)
      | t1 == t00 -> pure $ S.TypedTerm (S.Application (S.ApplicationC v0' v1')) t01
      | otherwise -> throwM $ MismatchException (show t00) $ show t1
    _ -> throwM $ MismatchException "function" $ show t0
typeOfTerm ctx (S.UntypedValue (S.Procedure (p:|ps))) = do
  (ctx', p') <- typeOfProcedure ctx p
  (_, ps') <- foldr go (pure (ctx', [])) ps
  let ps'' = p':|ps'
  case N.last ps'' of
    S.TermProcedure (S.TypedTerm _ t) -> pure $ S.TypedTerm (S.Procedure ps'') t
    S.BindProcedure {}                -> throwM BindProcedureEndException
  where
    go p acc = do
      (ctx, ps) <- acc
      (ctx', p') <- typeOfProcedure ctx p
      pure (ctx', p':ps)
typeOfTerm ctx (S.UntypedValue (S.TypeAnnotation (S.TypeAnnotation' v t))) = do
  v'@(S.TypedTerm _ t') <- typeOfTerm ctx v
  if t == t'
    then pure v'
    else throwM $ MismatchException (show t) (show t')
typeOfTerm ctx (S.UntypedValue (S.Let ds v)) = do
  ds' <- definitions ctx ds
  let ctx' = ((\(S.TypedTerm _ t) -> t) <$> M.fromList (mapMaybe valueBind ds')) `M.union` ctx
  v'@(S.TypedTerm _ t') <- typeOfTerm ctx' v
  pure $ S.TypedTerm (S.Let ds' v') t'

typeOfProcedure :: (MonadThrow m, MonadCatch m) => Map S.QualifiedIdentifier Type -> ProcedureStep 'S.Untyped -> m (Map S.QualifiedIdentifier Type, ProcedureStep 'S.Typed)
typeOfProcedure ctx (S.BindProcedure i v) = do
  v'@(S.TypedTerm _ t) <- typeOfTerm ctx v
  let ctx' = M.insert i t ctx
  pure (ctx', S.BindProcedure i v')
typeOfProcedure ctx (S.TermProcedure v) = do
  v' <- typeOfTerm ctx v
  pure (ctx, S.TermProcedure v')

data Exception
  = NotFoundException S.QualifiedIdentifier
  | MismatchException { expected :: String, actual :: String}
  | BindProcedureEndException
  | RecursionException (Set S.QualifiedIdentifier)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
