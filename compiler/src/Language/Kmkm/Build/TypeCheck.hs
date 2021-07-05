{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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

type Module t f = S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t f

type Definition t f = S.Definition 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t f

type Type f = S.Type 'S.NameResolved 'S.Curried f

type Value t f = S.Value 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t f

type Value' t f = S.Value' 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t f

type ProcedureStep t f = S.ProcedureStep 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t f

typeCheck
  :: ( MonadThrow m
     , MonadCatch m
     , S.HasPosition f
     , Eq (f (S.Type 'S.NameResolved 'S.Curried f))
     , Show (f (S.Type 'S.NameResolved 'S.Curried f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (S.Type 'S.NameResolved 'S.Curried f))
  -> f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed f))
typeCheck = traverse . typeCheck'

typeCheck'
  :: ( MonadThrow m
     , MonadCatch m
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> Module 'S.Untyped f
  -> m (Module 'S.Typed f)
typeCheck' ctx (S.Module mn ms ds) = do
  ds' <- definitions ctx $ S.item <$> ds
  pure $ S.Module mn ms $ zipWith (<$) ds' ds

definitions  :: ( MonadThrow m
     , MonadCatch m
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     ) => Map S.QualifiedIdentifier (f (Type f)) -> [Definition 'S.Untyped f] -> m [Definition 'S.Typed f]
definitions context definitions' = do
  let
    valueBinds = M.fromList $ mapMaybe valueBind definitions'
    dependencyGraph = G.overlays $ dependency (M.keysSet valueBinds) <$> definitions'
    sortedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencyGraph
    context' = M.mapMaybe annotatedType (S.item <$> valueBinds) `M.union` context
  typedValueBinds <- foldr (typeBind context' valueBinds) (pure M.empty) sortedIdentifiers
  pure $ replaceTerm typedValueBinds <$> definitions'

dependency :: S.HasPosition f => Set S.QualifiedIdentifier -> Definition 'S.Untyped f -> G.AdjacencyMap S.QualifiedIdentifier
dependency valueBinds (S.ValueBind (S.ValueBindU i v)) = G.vertex (S.item i) `G.connect` G.overlays (G.vertex <$> dep valueBinds (S.item v))
dependency _ _                                    = G.empty

typeBind
  :: ( MonadThrow m
     , MonadCatch m
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
    (\e -> case e of { NotFoundException i _ -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        context' = ((\v -> let S.TypedValue _ t = S.item v in t) <$> typedValueBinds') `M.union` context
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- sequence $ traverse (typeOfTerm context') <$> recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throwM $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: S.HasPosition f => Value 'S.Untyped f -> Maybe (f (Type f))
annotatedType (S.UntypedValue v)
  | S.TypeAnnotation (S.TypeAnnotation' _ t) <- S.item v = Just t
annotatedType _                                                           = Nothing

replaceTerm :: S.HasPosition f => Map S.QualifiedIdentifier (f (Value 'S.Typed f)) -> Definition 'S.Untyped f -> Definition 'S.Typed f
replaceTerm _ (S.DataDefinition i cs) = S.DataDefinition i cs
replaceTerm _ (S.TypeBind i t) = S.TypeBind i t
replaceTerm _ (S.ForeignTypeBind i hs c) = S.ForeignTypeBind i hs c
replaceTerm typedValueBinds (S.ValueBind (S.ValueBindU i _)) =
  S.ValueBind (S.ValueBindU i $ fromMaybe unreachable $ M.lookup (S.item i) typedValueBinds)
replaceTerm _ (S.ForeignValueBind i hs c t) = S.ForeignValueBind i hs c t

valueBind :: S.HasPosition f => Definition t f -> Maybe (S.QualifiedIdentifier, f (Value t f))
valueBind (S.ValueBind (S.ValueBindU i v)) = Just (S.item i, v)
valueBind _                                = Nothing

dep :: S.HasPosition f => Set S.QualifiedIdentifier -> Value 'S.Untyped f -> [S.QualifiedIdentifier]
dep identifiers =
  liftValue $ dep' . S.item
  where
    dep' (S.Variable i)
      | i `S.member` identifiers = [i]
      | otherwise = []
    dep' (S.Literal (S.Function (S.FunctionC i _ v'))) = dep (S.delete (S.item i) identifiers) $ S.item v'
    dep' (S.Literal _) = []
    dep' (S.Application (S.ApplicationC v1 v2)) = mconcat $ dep identifiers . S.item <$> [v1, v2]
    dep' (S.Procedure ps) =
      fst $ foldl' go ([], identifiers) ps
      where
        go (is, ss) v =
          case S.item v of
            S.BindProcedure i v' -> (dep ss (S.item v') ++ is, S.delete (S.item i) ss)
            S.TermProcedure v'   -> (dep ss (S.item v') ++ is, ss)
    dep' (S.TypeAnnotation (S.TypeAnnotation' v' _)) = dep identifiers $ S.item v'
    dep' (S.Let ds v) =
      let
        valueBinds = M.fromList $ mapMaybe (valueBind . S.item) ds
        identifiers' = identifiers S.\\ M.keysSet valueBinds
      in dep identifiers' . S.item =<< v : M.elems valueBinds

typeOfTerm
  :: ( MonadThrow m
     , MonadCatch m
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> Value 'S.Untyped f
  -> m (Value 'S.Typed f)
typeOfTerm ctx =
  liftValue typeOfTerm'
  where
    typeOfTerm' v =
      case S.item v of
        S.Variable i ->
          case M.lookup i ctx of
            Nothing -> throwM $ NotFoundException i $ S.range v
            Just t  -> pure $ S.TypedValue (S.Variable i <$ v) t
        S.Literal (S.Integer v' b) ->
          pure $ S.TypedValue (S.Literal (S.Integer v' b) <$ v) (S.TypeVariable (S.GlobalIdentifier ["kmkm", "prim"] "int" <$ v) <$ v)
        S.Literal (S.Fraction s d e b) ->
          pure $ S.TypedValue (S.Literal (S.Fraction s d e b) <$ v) (S.TypeVariable (S.GlobalIdentifier ["kmkm", "prim"] "frac2" <$ v) <$ v)
        S.Literal (S.String t) ->
          pure $ S.TypedValue (S.Literal (S.String t) <$ v) (S.TypeVariable (S.GlobalIdentifier ["kmkm", "prim"] "string" <$ v) <$ v)
        S.Literal (S.Function (S.FunctionC i t v')) -> do
          v''@(S.TypedValue _ t') <- typeOfTerm (M.insert (S.item i) t ctx) (S.item v')
          pure $ S.TypedValue (S.Literal (S.Function (S.FunctionC i t (v'' <$ v'))) <$ v) (S.FunctionType (S.FunctionTypeC t t') <$ v)
        S.Application (S.ApplicationC v0 v1) -> do
          v0'@(S.TypedValue _ t0) <- typeOfTerm ctx $ S.item v0
          v1'@(S.TypedValue _ t1) <- typeOfTerm ctx $ S.item v1
          case S.item t0 of
            S.FunctionType (S.FunctionTypeC t00 t01)
              | t1 == t00 -> pure $ S.TypedValue (S.Application (S.ApplicationC (v0' <$ v0) $ v1' <$ v1) <$ v) t01
              | otherwise -> throwM $ MismatchException (show $ S.item t00) $ show t1
            _ -> throwM $ MismatchException "function" $ show t0
        S.Procedure (p:|ps) -> do
          (ctx', p') <- typeOfProcedure ctx $ S.item p
          (_, ps') <- foldr go (pure (ctx', [])) ps
          let ps'' = (p' <$ p):|ps'
          case S.item $ N.last ps'' of
            S.TermProcedure v ->
              let S.TypedValue _ t = S.item v
              in pure $ S.TypedValue (S.Procedure ps'' <$ v) t
            S.BindProcedure {} -> throwM BindProcedureEndException
          where
            go p acc = do
              (ctx, ps) <- acc
              (ctx', p') <- typeOfProcedure ctx $ S.item p
              pure (ctx', (p' <$ p):ps)
        S.TypeAnnotation (S.TypeAnnotation' v' t) -> do
          v''@(S.TypedValue _ t') <- typeOfTerm ctx $ S.item v'
          if t == t'
            then pure v''
            else throwM $ MismatchException (show t) (show t')
        S.Let ds v' -> do
          ds' <- definitions ctx $ S.item <$> ds
          let ctx' = ((\(S.TypedValue _ t) -> t) . S.item <$> M.fromList (mapMaybe valueBind ds')) `M.union` ctx
          v''@(S.TypedValue _ t') <- typeOfTerm ctx' $ S.item v'
          pure $ S.TypedValue (S.Let (zipWith (<$) ds' ds) (v'' <$ v') <$ v) t'

typeOfProcedure
  :: ( MonadThrow m
     , MonadCatch m
     , S.HasPosition f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
  )
  => Map S.QualifiedIdentifier (f (Type f))
  -> ProcedureStep 'S.Untyped f
  -> m (Map S.QualifiedIdentifier (f (Type f)), ProcedureStep 'S.Typed f)
typeOfProcedure ctx (S.BindProcedure i v) = do
  v'@(S.TypedValue _ t) <- typeOfTerm ctx $ S.item v
  let ctx' = M.insert (S.item i) t ctx
  pure (ctx', S.BindProcedure i (v' <$ v))
typeOfProcedure ctx (S.TermProcedure v) = do
  v' <- typeOfTerm ctx $ S.item v
  pure (ctx, S.TermProcedure (v' <$ v))

liftValue :: (f (Value' 'S.Untyped f) -> a) -> Value 'S.Untyped f -> a
liftValue f (S.UntypedValue v) = f v

data Exception
  = NotFoundException S.QualifiedIdentifier (Maybe (S.Position, S.Position))
  | MismatchException { expected :: String, actual :: String}
  | BindProcedureEndException
  | RecursionException (Set S.QualifiedIdentifier)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
