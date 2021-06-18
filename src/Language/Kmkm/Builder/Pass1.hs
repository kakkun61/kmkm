{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors      #-}
#else
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

-- | \"Type check\" pass.
module Language.Kmkm.Builder.Pass1
  ( typeCheck
  , Exception (..)
  ) where

import           Language.Kmkm.Exception     (unreachable)
import qualified Language.Kmkm.Exception     as X
import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (ModuleName, QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax.Phase1 as P1
import qualified Language.Kmkm.Syntax.Phase2 as P2
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

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

typeCheck :: (MonadThrow m, MonadCatch m) => Map QualifiedIdentifier P1.Type -> P1.Module -> m P2.Module
typeCheck ctx (S.Module mn ds ms) = S.Module mn ds <$> members mn ctx ms

members :: (MonadThrow m, MonadCatch m) => ModuleName -> Map QualifiedIdentifier P1.Type -> [P1.Member] -> m [P2.Member]
members mn ctx ms = do
  let
    siblings = rootIdentifiers mn ms
    dependencies =
      G.overlays $ go <$> ms
      where
        go (S.ValueBind (S.ValueBindU i v) ms) = G.vertex (QualifiedIdentifier (Just mn) i) `G.connect` G.overlays (G.vertex <$> dependency mn (M.keysSet siblings) v ms)
        go (S.ForeignValueBind i _ _ _)        = G.vertex $ QualifiedIdentifier (Just mn) i
        go _                                   = G.empty
    orderedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencies
  typedSiblings <-
    let
      go :: (MonadThrow m, MonadCatch m) => GN.AdjacencyMap QualifiedIdentifier -> m (Map QualifiedIdentifier P2.Term) -> m (Map QualifiedIdentifier P2.Term)
      go ks acc = do
        let ks' = GN.vertexList1 ks
        case sequence $ (\k -> (k,) <$> M.lookup k siblings) <$> ks' of
          Nothing -> acc
          Just bs -> do
            acc' <- acc
            let
              ks = fst <$> bs
              ctx' =
                foldr go ((typ <$> acc') `M.union` ctx) bs
                where
                  go (k, V.UntypedTerm (V.TypeAnnotation (V.TypeAnnotation' _ t))) c = M.insert k t c
                  go _ c                                                             = c
              go (k, v) acc = M.insert k <$> typeOfTerm mn ctx' v <*> acc
            catchJust
              (\e -> case e of { NotFoundException i -> if i `elem` ks then Just i else Nothing; _ -> Nothing })
              (foldr go acc bs)
              $ const $ throwM $ RecursionException $ S.fromList $ N.toList ks'
      typ :: P2.Term -> P2.Type
      typ (V.TypedTerm _ t) = t
    in foldr go (pure M.empty) orderedIdentifiers
  let
    go (S.Definition i cs)                 = pure $ S.Definition i cs
    go (S.TypeBind i t)                    = pure $ S.TypeBind i t
    go (S.ValueBind (S.ValueBindU i _) ms) = S.ValueBind (S.ValueBindU i $ fromMaybe unreachable $ M.lookup (QualifiedIdentifier (Just mn) i) typedSiblings) <$> members mn ctx ms
    go (S.ForeignValueBind i hs c t)       = pure $ S.ForeignValueBind i hs c t
  sequence $ go <$> ms

rootIdentifiers :: ModuleName -> [P1.Member] -> Map QualifiedIdentifier P1.Term
rootIdentifiers mn ms =
  M.fromList $ mapMaybe go ms
  where
    go (S.ValueBind (S.ValueBindU i v) _) = Just (QualifiedIdentifier (Just mn) i, v)
    go _                                  = Nothing

dependency :: ModuleName -> Set QualifiedIdentifier -> P1.Term -> [P1.Member] -> [QualifiedIdentifier]
dependency mn siblings v ms =
  let subSiblings = M.keysSet $ rootIdentifiers mn ms
  in
    mconcat
      [ dep siblings v
      , ms >>= \m ->
          case m of
            S.ValueBind (S.ValueBindU _ v) ms ->
              dependency mn (siblings S.\\ subSiblings) v ms
            _ -> []
      ]
  where
    dep :: Set QualifiedIdentifier -> P1.Term -> [QualifiedIdentifier]
    dep siblings (V.UntypedTerm (V.Variable i))
      | i `S.member` siblings = [i]
      | otherwise             = []
    dep siblings (V.UntypedTerm (V.Literal (V.Function (V.FunctionC i _ v)))) = dep (S.delete (QualifiedIdentifier Nothing i) siblings) v
    dep _ (V.UntypedTerm (V.Literal _)) = []
    dep siblings (V.UntypedTerm (V.Application (V.ApplicationC v1 v2))) = mconcat $ dep siblings <$> [v1, v2]
    dep siblings (V.UntypedTerm (V.Procedure ps)) =
      fst $ foldl' go ([], siblings) ps
      where
        go (is, ss) (V.BindProcedure i v) = (dep ss v ++ is, S.delete (QualifiedIdentifier (Just mn) i) ss)
        go (is, ss) (V.TermProcedure v)   = (dep ss v ++ is, ss)
    dep siblings (V.UntypedTerm (V.TypeAnnotation (V.TypeAnnotation' v _))) = dep siblings v

typeOfTerm :: MonadThrow m => ModuleName -> Map QualifiedIdentifier P1.Type -> P1.Term -> m P2.Term
typeOfTerm _ ctx (V.UntypedTerm (V.Variable i)) =
  case M.lookup i ctx of
    Nothing -> throwM $ NotFoundException i
    Just t  -> pure $ V.TypedTerm (V.Variable i) t
typeOfTerm _ _ (V.UntypedTerm (V.Literal (V.Integer v b))) = pure $ V.TypedTerm (V.Literal (V.Integer v b)) (T.Variable "int")
typeOfTerm _ _ (V.UntypedTerm (V.Literal (V.Fraction s d e b))) = pure $ V.TypedTerm (V.Literal (V.Fraction s d e b)) (T.Variable "frac2")
typeOfTerm _ _ (V.UntypedTerm (V.Literal (V.String t))) = pure $ V.TypedTerm (V.Literal (V.String t)) (T.Variable "string")
typeOfTerm mn ctx (V.UntypedTerm (V.Literal (V.Function (V.FunctionC i t v)))) = do
  v'@(V.TypedTerm _ t') <- typeOfTerm mn (M.insert (QualifiedIdentifier Nothing i) t ctx) v
  pure $ V.TypedTerm (V.Literal (V.Function (V.FunctionC i t v'))) (T.Function $ T.FunctionC t t')
typeOfTerm mn ctx (V.UntypedTerm (V.Application (V.ApplicationC v0 v1))) = do
  v0'@(V.TypedTerm _ t0) <- typeOfTerm mn ctx v0
  v1'@(V.TypedTerm _ t1) <- typeOfTerm mn ctx v1
  case t0 of
    T.Function (T.FunctionC t00 t01)
      | t1 == t00 -> pure $ V.TypedTerm (V.Application (V.ApplicationC v0' v1')) t01
      | otherwise -> throwM $ MismatchException (show t00) $ show t1
    _ -> throwM $ MismatchException "arrow" $ show t0
typeOfTerm mn ctx (V.UntypedTerm (V.Procedure (p:|ps))) = do
  (ctx', p') <- typeOfProcedure mn ctx p
  (_, ps') <- foldr go (pure (ctx', [])) ps
  let ps'' = p':|ps'
  case N.last ps'' of
    V.TermProcedure (V.TypedTerm _ t) -> pure $ V.TypedTerm (V.Procedure ps'') t
    V.BindProcedure {}                -> throwM BindProcedureEndException
  where
    go p acc = do
      (ctx, ps) <- acc
      (ctx', p') <- typeOfProcedure mn ctx p
      pure (ctx', p':ps)
typeOfTerm mn ctx (V.UntypedTerm (V.TypeAnnotation (V.TypeAnnotation' v t))) = do
  v'@(V.TypedTerm _ t') <- typeOfTerm mn ctx v
  if t == t'
    then pure v'
    else throwM $ MismatchException (show t) (show t')

typeOfProcedure :: MonadThrow m => ModuleName -> Map QualifiedIdentifier P1.Type -> P1.ProcedureStep -> m (Map QualifiedIdentifier P1.Type, P2.ProcedureStep)
typeOfProcedure mn ctx (V.BindProcedure i v) = do
  v'@(V.TypedTerm _ t) <- typeOfTerm mn ctx v
  let ctx' = M.insert (QualifiedIdentifier (Just mn) i) t ctx
  pure (ctx', V.BindProcedure i v')
typeOfProcedure mn ctx (V.TermProcedure v) = do
  v' <- typeOfTerm mn ctx v
  pure (ctx, V.TermProcedure v')

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
