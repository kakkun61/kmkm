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
import           Language.Kmkm.Syntax.Base   (Identifier)
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

typeCheck :: (MonadThrow m, MonadCatch m) => P1.Module -> m P2.Module
typeCheck (S.Module i ms) = S.Module i <$> members ms

members :: (MonadThrow m, MonadCatch m) => [P1.Member] -> m [P2.Member]
members ms = do
  let
    siblings = rootIdentifiers ms
    dependencies =
      gsum $ go <$> ms
      where
        go (S.Bind (S.TermBind (S.TermBindU i v) ms)) = G.vertex i `G.connect` gsum (G.vertex <$> dependency (M.keysSet siblings) v ms)
        go _                                          = G.empty
        gsum :: (Foldable f, Ord a) => f (G.AdjacencyMap a) -> G.AdjacencyMap a
        gsum = foldr G.overlay G.empty
    orderedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencies
  typedSiblings <-
    let
      go :: (MonadThrow m, MonadCatch m) => GN.AdjacencyMap Identifier -> m (Map Identifier P2.Term) -> m (Map Identifier P2.Term)
      go ks acc =
        case G.vertexList ks of
          [] -> X.unreachable
          ks ->
            case sequence $ (\k -> (k,) <$> M.lookup k siblings) <$> ks of
              Nothing -> X.unreachable
              Just bs -> do
                acc' <- acc
                let
                  ks = fst <$> bs
                  ctx =
                    foldr go (typ <$> acc') bs
                    where
                      go (k, V.UntypedTerm (V.TypeAnnotation (V.TypeAnnotation' _ t))) c = M.insert k t c
                      go _ c                                                             = c
                  go (k, v) acc = M.insert k <$> typeOfTerm ctx v <*> acc
                catchJust
                  (\e -> case e of { NotFoundException i -> if i `elem` ks then Just i else Nothing; _ -> Nothing })
                  (foldr go acc bs)
                  $ const $ throwM $ RecursionException $ S.fromList ks
      typ :: P2.Term -> P2.Type
      typ (V.TypedTerm _ t) = t
    in foldr go (pure M.empty) orderedIdentifiers
  let
    go (S.Definition i cs)                        = pure $ S.Definition i cs
    go (S.Bind (S.TypeBind i t))                  = pure $ S.Bind $ S.TypeBind i t
    go (S.Bind (S.TermBind (S.TermBindU i _) ms)) = S.Bind . S.TermBind (S.TermBindU i $ fromMaybe X.unreachable $ M.lookup i typedSiblings) <$> members ms
  sequence $ go <$> ms

rootIdentifiers :: [P1.Member] -> Map Identifier P1.Term
rootIdentifiers ms =
  M.fromList $ mapMaybe go ms
  where
    go (S.Bind (S.TermBind (S.TermBindU i v) _)) = Just (i, v)
    go _                                         = Nothing

dependency :: Set Identifier -> P1.Term -> [P1.Member] -> [Identifier]
dependency siblings v ms =
  let subSiblings = M.keysSet $ rootIdentifiers ms
  in
    mconcat
      [ dep siblings v
      , ms >>= \m ->
          case m of
            S.Bind (S.TermBind (S.TermBindU _ v) ms) ->
              dependency (siblings `S.difference` subSiblings) v ms
            _ -> []
      ]
  where
    dep :: Set Identifier -> P1.Term -> [Identifier]
    dep siblings (V.UntypedTerm (V.Variable i))
      | i `S.member` siblings = [i]
      | otherwise             = []
    dep siblings (V.UntypedTerm (V.Literal (V.Function (V.FunctionC i _ v)))) = dep (S.delete i siblings) v
    dep _ (V.UntypedTerm (V.Literal _)) = []
    dep siblings (V.UntypedTerm (V.Application (V.ApplicationC v1 v2))) = mconcat $ dep siblings <$> [v1, v2]
    dep siblings (V.UntypedTerm (V.Procedure ps)) =
      fst $ foldl' go ([], siblings) ps
      where
        go (is, ss) (V.BindProcedure i v) = (dep ss v ++ is, S.delete i ss)
        go (is, ss) (V.TermProcedure v)   = (dep ss v ++ is, ss)
    dep siblings (V.UntypedTerm (V.TypeAnnotation (V.TypeAnnotation' v _))) = dep siblings v

typeOfTerm :: MonadThrow m => Map Identifier P1.Type -> P1.Term -> m P2.Term
typeOfTerm ctx (V.UntypedTerm (V.Variable i)) =
  case M.lookup i ctx of
    Nothing -> throwM $ NotFoundException i
    Just t  -> pure $ V.TypedTerm (V.Variable i) t
typeOfTerm _ (V.UntypedTerm (V.Literal (V.Integer v b))) = pure $ V.TypedTerm (V.Literal (V.Integer v b)) (T.Variable "int")
typeOfTerm _ (V.UntypedTerm (V.Literal (V.Fraction s d e b))) = pure $ V.TypedTerm (V.Literal (V.Fraction s d e b)) (T.Variable "frac2")
typeOfTerm _ (V.UntypedTerm (V.Literal (V.String t))) = pure $ V.TypedTerm (V.Literal (V.String t)) (T.Variable "string")
typeOfTerm ctx (V.UntypedTerm (V.Literal (V.Function (V.FunctionC i t v)))) = do
  v'@(V.TypedTerm _ t') <- typeOfTerm (M.insert i t ctx) v
  pure $ V.TypedTerm (V.Literal (V.Function (V.FunctionC i t v'))) (T.Arrow $ T.ArrowC t t')
typeOfTerm ctx (V.UntypedTerm (V.Application (V.ApplicationC v0 v1))) = do
  v0'@(V.TypedTerm _ t0) <- typeOfTerm ctx v0
  v1'@(V.TypedTerm _ t1) <- typeOfTerm ctx v1
  case t0 of
    T.Arrow (T.ArrowC t00 t01)
      | t1 == t00 -> pure $ V.TypedTerm (V.Application (V.ApplicationC v0' v1')) t01
      | otherwise -> throwM $ MismatchException (show t00) $ show t1
    _ -> throwM $ MismatchException "arrow" $ show t0
typeOfTerm ctx (V.UntypedTerm (V.Procedure (p:|ps))) = do
  (ctx', p') <- typeOfProcedure ctx p
  (_, ps') <- foldr go (pure (ctx', [])) ps
  let ps'' = p':|ps'
  case N.last ps'' of
    V.TermProcedure (V.TypedTerm _ t) -> pure $ V.TypedTerm (V.Procedure ps'') t
    V.BindProcedure {}                -> throwM BindProcedureEndException
  where
    go p acc = do
      (ctx, ps) <- acc
      (ctx', p') <- typeOfProcedure ctx p
      pure (ctx', p':ps)
typeOfTerm ctx (V.UntypedTerm (V.TypeAnnotation (V.TypeAnnotation' v t))) = do
  v'@(V.TypedTerm _ t') <- typeOfTerm ctx v
  if t == t'
    then pure v'
    else throwM $ MismatchException (show t) (show t')

typeOfProcedure :: MonadThrow m => Map Identifier P1.Type -> P1.ProcedureStep -> m (Map Identifier P1.Type, P2.ProcedureStep)
typeOfProcedure ctx (V.BindProcedure i v) = do
  v'@(V.TypedTerm _ t) <- typeOfTerm ctx v
  let ctx' = M.insert i t ctx
  pure (ctx', V.BindProcedure i v')
typeOfProcedure ctx (V.TermProcedure v) = do
  v' <- typeOfTerm ctx v
  pure (ctx, V.TermProcedure v')

data Exception
  = NotFoundException Identifier
  | MismatchException { expected :: String, actual :: String}
  | BindProcedureEndException
  | RecursionException (Set Identifier)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
