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

#if __GLASGOW_HASKELL__ < 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

-- | \"Type check\" pass.
module Language.Kmkm.Internal.Build.TypeCheck
  ( typeCheck
  , Exception (..)
  ) where

import           Language.Kmkm.Internal.Exception (unreachable)
import qualified Language.Kmkm.Internal.Exception as X
import qualified Language.Kmkm.Internal.Syntax    as S

import qualified Algebra.Graph.AdjacencyMap           as G hiding (vertexList)
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G hiding (topSort)
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Algebra.Graph.ToGraph                as G
import qualified Barbies.Bare                         as B (Bare, Covered)
import qualified Barbies.Bare.Layered                 as B (BareB)
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch, MonadThrow, catchJust, throw)
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

type Module t et ev f = S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev B.Covered f

type Definition t et ev f = S.Definition 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev B.Covered f

type Type f = S.Type 'S.NameResolved 'S.Curried B.Covered f

type Value t et ev f = S.Value 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev B.Covered f

type ProcedureStep t et ev f = S.ProcedureStep 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev B.Covered f

typeCheck
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , Eq (f (S.Type 'S.NameResolved 'S.Curried B.Covered f))
     , Show (f (S.Type 'S.NameResolved 'S.Curried B.Covered f))
     , Show (f S.QualifiedIdentifier)
     , B.BareB et
     , B.BareB ev
     )
  => Map S.QualifiedIdentifier (f (S.Type 'S.NameResolved 'S.Curried B.Covered f))
  -> f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed et ev B.Covered f))
typeCheck = typeCheck'

typeCheck'
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     , B.BareB et
     , B.BareB ev)
  => Map S.QualifiedIdentifier (f (Type f))
  -> f (Module 'S.Untyped et ev f)
  -> m (f (Module 'S.Typed et ev f))
typeCheck' ctx m =
  traverse go m
  where
    prim = primitivesImporting m
    go (S.Module mn ms ds) = S.Module mn ms <$> definitions ctx prim ds

{-# ANN definitions ("HLint: ignore Use list literal" :: String) #-}

definitions
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> PrimitivesImported
  -> f [f (Definition 'S.Untyped et ev f)]
  -> m (f [f (Definition 'S.Typed et ev f)])
definitions context prim =
  traverse go
  where
    go definitions' = do
      let
        valueBinds = M.fromList $ mapMaybe valueBind definitions'
        foreignValueBinds = M.fromList $ mapMaybe foreignValueBind definitions'
        dependencyGraph = G.overlays $ dependency (M.keysSet valueBinds) <$> definitions'
        sortedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencyGraph
        context' = M.unions $ M.mapMaybe annotatedType valueBinds : foreignValueBinds : context : []
      typedValueBinds <- foldr (flip $ typeBind context' valueBinds prim) (pure M.empty) sortedIdentifiers
      pure $ replaceValue typedValueBinds <$> definitions'

dependency :: (Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f (Definition 'S.Untyped et ev f) -> G.AdjacencyMap S.QualifiedIdentifier
dependency valueBinds d | S.ValueBind (S.ValueBindU i v) <- copoint d = G.vertex (copoint i) `G.connect` G.overlays (G.vertex <$> dep valueBinds v)
dependency _ _                                                        = G.empty

typeBind
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> Map S.QualifiedIdentifier (f (Value 'S.Untyped et ev f))
  -> PrimitivesImported
  -> m (Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)))
  -> GN.AdjacencyMap S.QualifiedIdentifier
  -> m (Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)))
typeBind context valueBinds prim typedValueBinds recursionIdentifiers =
  catchJust
    (\case { NotFoundException i _ -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        context' = ((\v -> let S.TypedValue _ t = copoint v in t) <$> typedValueBinds') `M.union` context
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- sequence $ typeOfTerm context' prim <$> recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throw $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: Copointed f => f (Value 'S.Untyped et ev f) -> Maybe (f (Type f))
annotatedType v
  | S.UntypedValue v <- copoint v
  , S.TypeAnnotation (S.TypeAnnotation' _ t) <- copoint v = Just t
annotatedType _ = Nothing

replaceValue :: (Functor f, Copointed f) => Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)) -> f (Definition 'S.Untyped et ev f) -> f (Definition 'S.Typed et ev f)
replaceValue typedValueBinds =
  fmap $ \case
    S.DataDefinition i cs -> S.DataDefinition i cs
    S.TypeBind i t -> S.TypeBind i t
    S.ForeignTypeBind i c -> S.ForeignTypeBind i c
    S.ValueBind (S.ValueBindU i _) ->
      S.ValueBind (S.ValueBindU i $ fromMaybe unreachable $ M.lookup (copoint i) typedValueBinds)
    S.ForeignValueBind i c t -> S.ForeignValueBind i c t

valueBind :: Copointed f => f (Definition t et ev f) -> Maybe (S.QualifiedIdentifier, f (Value t et ev f))
valueBind d | S.ValueBind (S.ValueBindU i v) <- copoint d = Just (copoint i, v)
valueBind _                                               = Nothing

foreignValueBind :: Copointed f => f (Definition t et ev f) -> Maybe (S.QualifiedIdentifier, f (Type f))
foreignValueBind d | S.ForeignValueBind i _ t <- copoint d       = Just (copoint i, t)
foreignValueBind _                                               = Nothing

dep :: (Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f (Value 'S.Untyped et ev f) -> [S.QualifiedIdentifier]
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
                S.BindProcedureStep i v' -> (dep ss v' ++ is, S.delete (copoint i) ss)
                S.CallProcedureStep v'   -> (dep ss v' ++ is, ss)
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
     , S.HasLocation f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (Type f))
  -> PrimitivesImported
  -> f (Value 'S.Untyped et ev f)
  -> m (f (Value 'S.Typed et ev f))
typeOfTerm ctx prim@PrimitivesImported { int = primInt, frac2 = primFrac2, string = primString } v =
  case copoint v of
    S.UntypedValue v' ->
      case copoint v' of
        S.Variable i ->
          let i' = copoint i
          in
            case M.lookup i' ctx of
              Nothing -> throw $ NotFoundException i' $ S.location i
              Just t  -> pure $ S.TypedValue (S.Variable i <$ v') t <$ v
        S.Literal (S.Integer v_ b) ->
          let int = S.GlobalIdentifier ["kmkm", "prim"] "int"
          in
            if primInt
              then pure $ S.TypedValue (S.Literal (S.Integer v_ b) <$ v') (S.TypeVariable (int <$ v) <$ v) <$ v
              else throw $ PrimitiveTypeException int $ S.location v
        S.Literal (S.Fraction s d e b) ->
          let frac2 = S.GlobalIdentifier ["kmkm", "prim"] "frac2"
          in
            if primFrac2
              then pure $ S.TypedValue (S.Literal (S.Fraction s d e b) <$ v') (S.TypeVariable (frac2 <$ v) <$ v) <$ v
              else throw $ PrimitiveTypeException frac2 $ S.location v
        S.Literal (S.String t) ->
          let string = S.GlobalIdentifier ["kmkm", "prim"] "string"
          in
            if primString
              then pure $ S.TypedValue (S.Literal (S.String t) <$ v') (S.TypeVariable (string <$ v) <$ v) <$ v
              else throw $ PrimitiveTypeException string $ S.location v
        S.Function (S.FunctionC i t v'') -> do
          v''' <- typeOfTerm (M.insert (copoint i) t ctx) prim v''
          let S.TypedValue _ t' = copoint v'''
          pure $ S.TypedValue (S.Function (S.FunctionC i t v''') <$ v') (S.FunctionType (S.FunctionTypeC t t') <$ v) <$ v
        S.Application (S.ApplicationC v0 v1) -> do
          v0' <- typeOfTerm ctx prim v0
          v1' <- typeOfTerm ctx prim v1
          let
            S.TypedValue _ t0 = copoint v0'
            S.TypedValue _ t1 = copoint v1'
          case copoint t0 of
            S.FunctionType (S.FunctionTypeC t00 t01)
              | S.strip t1 == S.strip t00 -> pure $ S.TypedValue (S.Application (S.ApplicationC v0' v1') <$ v') t01 <$ v
              | otherwise -> throw $ MismatchException (Right $ S.strip t00) (S.strip t1) $ S.location t1
            _ -> throw $ MismatchException (Left "function") (S.strip t0) $ S.location t0
        S.Procedure ps -> do
          let p :| ps' = copoint ps
          (ctx', p') <- typeOfProcedureStep ctx prim p
          (_, ps'') <- foldr go (pure (ctx', [])) ps'
          let ps''' = p' :| ps''
          case N.last ps''' of
            s
              | S.CallProcedureStep v <- copoint s ->
                  let S.TypedValue _ t = copoint v
                  in pure $ S.TypedValue (S.Procedure (ps''' <$ ps) <$ v') t <$ v
              | otherwise -> throw $ BindProcedureEndException $ S.location s
          where
            go p acc = do
              (ctx, ps) <- acc
              (ctx', p') <- typeOfProcedureStep ctx prim p
              pure (ctx', p' : ps)
        S.TypeAnnotation (S.TypeAnnotation' v' t) -> do
          v'' <- typeOfTerm ctx prim v'
          let S.TypedValue _ t' = copoint v''
          if S.strip t == S.strip t'
            then pure v''
            else throw $ MismatchException (Right $ S.strip t) (S.strip t') $ S.location t'
        S.Let ds v' -> do
          ds' <- definitions ctx prim ds
          let ctx' = ((\(S.TypedValue _ t) -> t) . copoint <$> M.fromList (mapMaybe valueBind $ copoint ds')) `M.union` ctx
          v'' <- typeOfTerm ctx' prim v'
          let S.TypedValue _ t' = copoint v''
          pure $ S.TypedValue (S.Let ds' v'' <$ v') t' <$ v

typeOfProcedureStep
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
  )
  => Map S.QualifiedIdentifier (f (Type f))
  -> PrimitivesImported
  -> f (ProcedureStep 'S.Untyped et ev f)
  -> m (Map S.QualifiedIdentifier (f (Type f)), f (ProcedureStep 'S.Typed et ev f))
typeOfProcedureStep ctx prim s =
  case copoint s of
    S.BindProcedureStep i v -> do
      v' <- typeOfTerm ctx prim v
      let
        S.TypedValue _ t = copoint v'
        ctx' = M.insert (copoint i) t ctx
      pure (ctx', S.BindProcedureStep i v' <$ s)
    S.CallProcedureStep v -> do
      v' <- typeOfTerm ctx prim v
      let S.TypedValue _ t = copoint v'
      case copoint t of
        S.ProcedureType _ -> pure (ctx, S.CallProcedureStep v' <$ s)
        _                 -> throw $ MismatchException (Left "procedure") (S.strip t) $ S.location t

primitivesImporting :: (Functor f, Copointed f, B.BareB et, B.BareB ev) => f (Module 'S.Untyped et ev f) -> PrimitivesImported
primitivesImporting m =
  case S.strip m of
    S.Module _ ms _ ->
      if ["kmkm", "prim"] `elem` ms
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
  = NotFoundException S.QualifiedIdentifier (Maybe S.Location)
  | MismatchException { expected :: Either Text (S.Type 'S.NameResolved 'S.Curried B.Bare Identity), actual :: S.Type 'S.NameResolved 'S.Curried B.Bare Identity, location :: Maybe S.Location }
  | BindProcedureEndException (Maybe S.Location)
  | RecursionException (Set S.QualifiedIdentifier)
  | PrimitiveTypeException S.QualifiedIdentifier (Maybe S.Location)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
