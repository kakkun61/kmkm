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

import           Language.Kmkm.Exception     (unreachable)
import qualified Language.Kmkm.Exception     as X
import           Language.Kmkm.Syntax        (Identifier, ModuleName, QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax        as S

import qualified Algebra.Graph.AdjacencyMap           as G hiding (vertexList)
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G hiding (topSort)
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Algebra.Graph.ToGraph                as G
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch)
import           Control.Monad.Catch                  (MonadThrow (throwM), catchJust)
import           Data.Bifunctor                       (Bifunctor (first))
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

type Module t = S.Module 'S.Curried 'S.LambdaUnlifted t

type Definition t = S.Definition 'S.Curried 'S.LambdaUnlifted t

type Type = S.Type 'S.Curried

type Term t = S.Term 'S.Curried 'S.LambdaUnlifted t

type ProcedureStep t = S.ProcedureStep 'S.Curried 'S.LambdaUnlifted t

typeCheck :: (MonadThrow m, MonadCatch m) => Map QualifiedIdentifier Type -> Module 'S.Untyped -> m (Module 'S.Typed)
typeCheck ctx (S.Module mn ds ms) = S.Module mn ds <$> definitions mn ctx ms

definitions :: (MonadThrow m, MonadCatch m) => ModuleName -> Map QualifiedIdentifier Type -> [Definition 'S.Untyped] -> m [Definition 'S.Typed]
definitions moduleName context definitions' = do
  let
    valueBinds = M.fromList $ first (QualifiedIdentifier $ Just moduleName) <$> mapMaybe valueBind definitions'
    dependencyGraph = G.overlays $ dependency moduleName (M.keysSet valueBinds) <$> definitions'
    sortedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencyGraph
    context' = M.mapMaybe annotatedType valueBinds `M.union` context
  typedValueBinds <- foldr (typeBind moduleName context' valueBinds) (pure M.empty) sortedIdentifiers
  pure $ replaceTerm moduleName typedValueBinds <$> definitions'

dependency :: ModuleName -> Set QualifiedIdentifier -> Definition 'S.Untyped -> G.AdjacencyMap QualifiedIdentifier
dependency mn valueBinds (S.ValueBind (S.BindU i v)) = G.vertex (QualifiedIdentifier (Just mn) i) `G.connect` G.overlays (G.vertex <$> dep mn valueBinds v)
dependency _ _ _                                     = G.empty

typeBind
  :: (MonadThrow m, MonadCatch m)
  => ModuleName
  -> Map QualifiedIdentifier Type
  -> Map QualifiedIdentifier (Term 'S.Untyped)
  -> GN.AdjacencyMap QualifiedIdentifier
  -> m (Map QualifiedIdentifier (Term 'S.Typed))
  -> m (Map QualifiedIdentifier (Term 'S.Typed))
typeBind moduleName context valueBinds recursionIdentifiers typedValueBinds =
  catchJust
    (\e -> case e of { NotFoundException i -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        context' = ((\(S.TypedTerm _ t) -> t) <$> typedValueBinds') `M.union` context
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- sequence $ typeOfTerm moduleName context' <$> recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throwM $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: Term 'S.Untyped -> Maybe Type
annotatedType (S.UntypedTerm (S.TypeAnnotation (S.TypeAnnotation' _ t))) = Just t
annotatedType _                                                          = Nothing

replaceTerm :: ModuleName -> Map QualifiedIdentifier (Term 'S.Typed) -> Definition 'S.Untyped -> Definition 'S.Typed
replaceTerm _ _ (S.DataDefinition i cs) = S.DataDefinition i cs
replaceTerm _ _ (S.TypeBind i t) = S.TypeBind i t
replaceTerm moduleName typedValueBinds (S.ValueBind (S.BindU i _)) =
  S.ValueBind (S.BindU i $ fromMaybe unreachable $ M.lookup (QualifiedIdentifier (Just moduleName) i) typedValueBinds)
replaceTerm _ _ (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c t

valueBind :: S.Definition 'S.Curried 'S.LambdaUnlifted t -> Maybe (Identifier, S.Term 'S.Curried 'S.LambdaUnlifted t)
valueBind (S.ValueBind (S.BindU i v)) = Just (i, v)
valueBind _                           = Nothing

dep :: ModuleName -> Set QualifiedIdentifier -> Term 'S.Untyped -> [QualifiedIdentifier]
dep _ identifiers (S.UntypedTerm (S.Variable i))
  | i `S.member` identifiers = [i]
  | otherwise                = []
dep moduleName identifiers (S.UntypedTerm (S.Literal (S.Function (S.FunctionC i _ v)))) = dep moduleName (S.delete (QualifiedIdentifier Nothing i) identifiers) v
dep _ _ (S.UntypedTerm (S.Literal _)) = []
dep moduleName identifiers (S.UntypedTerm (S.Application (S.ApplicationC v1 v2))) = mconcat $ dep moduleName identifiers <$> [v1, v2]
dep moduleName identifiers (S.UntypedTerm (S.Procedure ps)) =
  fst $ foldl' go ([], identifiers) ps
  where
    go (is, ss) (S.BindProcedure i v) = (dep moduleName ss v ++ is, S.delete (QualifiedIdentifier (Just moduleName) i) ss)
    go (is, ss) (S.TermProcedure v)   = (dep moduleName ss v ++ is, ss)
dep moduleName identifiers (S.UntypedTerm (S.TypeAnnotation (S.TypeAnnotation' v _))) = dep moduleName identifiers v
dep moduleName identifiers (S.UntypedTerm (S.Let ds v)) =
  let
    valueBinds = M.fromList $ first (QualifiedIdentifier Nothing) <$> mapMaybe valueBind ds
    identifiers' = identifiers S.\\ M.keysSet valueBinds
  in dep moduleName identifiers' =<< v : M.elems valueBinds

typeOfTerm :: (MonadThrow m, MonadCatch m) => ModuleName -> Map QualifiedIdentifier Type -> Term 'S.Untyped -> m (Term 'S.Typed)
typeOfTerm _ ctx (S.UntypedTerm (S.Variable i)) =
  case M.lookup i ctx of
    Nothing -> throwM $ NotFoundException i
    Just t  -> pure $ S.TypedTerm (S.Variable i) t
typeOfTerm _ _ (S.UntypedTerm (S.Literal (S.Integer v b))) = pure $ S.TypedTerm (S.Literal (S.Integer v b)) (S.TypeVariable "int")
typeOfTerm _ _ (S.UntypedTerm (S.Literal (S.Fraction s d e b))) = pure $ S.TypedTerm (S.Literal (S.Fraction s d e b)) (S.TypeVariable "frac2")
typeOfTerm _ _ (S.UntypedTerm (S.Literal (S.String t))) = pure $ S.TypedTerm (S.Literal (S.String t)) (S.TypeVariable "string")
typeOfTerm mn ctx (S.UntypedTerm (S.Literal (S.Function (S.FunctionC i t v)))) = do
  v'@(S.TypedTerm _ t') <- typeOfTerm mn (M.insert (QualifiedIdentifier Nothing i) t ctx) v
  pure $ S.TypedTerm (S.Literal (S.Function (S.FunctionC i t v'))) (S.FunctionType $ S.FunctionTypeC t t')
typeOfTerm mn ctx (S.UntypedTerm (S.Application (S.ApplicationC v0 v1))) = do
  v0'@(S.TypedTerm _ t0) <- typeOfTerm mn ctx v0
  v1'@(S.TypedTerm _ t1) <- typeOfTerm mn ctx v1
  case t0 of
    S.FunctionType (S.FunctionTypeC t00 t01)
      | t1 == t00 -> pure $ S.TypedTerm (S.Application (S.ApplicationC v0' v1')) t01
      | otherwise -> throwM $ MismatchException (show t00) $ show t1
    _ -> throwM $ MismatchException "function" $ show t0
typeOfTerm mn ctx (S.UntypedTerm (S.Procedure (p:|ps))) = do
  (ctx', p') <- typeOfProcedure mn ctx p
  (_, ps') <- foldr go (pure (ctx', [])) ps
  let ps'' = p':|ps'
  case N.last ps'' of
    S.TermProcedure (S.TypedTerm _ t) -> pure $ S.TypedTerm (S.Procedure ps'') t
    S.BindProcedure {}                -> throwM BindProcedureEndException
  where
    go p acc = do
      (ctx, ps) <- acc
      (ctx', p') <- typeOfProcedure mn ctx p
      pure (ctx', p':ps)
typeOfTerm mn ctx (S.UntypedTerm (S.TypeAnnotation (S.TypeAnnotation' v t))) = do
  v'@(S.TypedTerm _ t') <- typeOfTerm mn ctx v
  if t == t'
    then pure v'
    else throwM $ MismatchException (show t) (show t')
typeOfTerm mn ctx (S.UntypedTerm (S.Let ds v)) = do
  ds' <- definitions mn ctx ds
  let ctx' = ((\(S.TypedTerm _ t) -> t) <$> M.fromList (first (QualifiedIdentifier Nothing) <$> mapMaybe valueBind ds')) `M.union` ctx
  v'@(S.TypedTerm _ t') <- typeOfTerm mn ctx' v
  pure $ S.TypedTerm (S.Let ds' v') t'

typeOfProcedure :: (MonadThrow m, MonadCatch m) => ModuleName -> Map QualifiedIdentifier Type -> ProcedureStep 'S.Untyped -> m (Map QualifiedIdentifier Type, ProcedureStep 'S.Typed)
typeOfProcedure mn ctx (S.BindProcedure i v) = do
  v'@(S.TypedTerm _ t) <- typeOfTerm mn ctx v
  let ctx' = M.insert (QualifiedIdentifier (Just mn) i) t ctx
  pure (ctx', S.BindProcedure i v')
typeOfProcedure mn ctx (S.TermProcedure v) = do
  v' <- typeOfTerm mn ctx v
  pure (ctx, S.TermProcedure v')

data Exception
  = NotFoundException QualifiedIdentifier
  | MismatchException { expected :: String, actual :: String}
  | BindProcedureEndException
  | RecursionException (Set QualifiedIdentifier)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
