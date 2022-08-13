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
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch, MonadThrow, catchJust, throw)
import           Data.Copointed                       (Copointed (copoint))
import           Data.Either                          (fromRight)
import qualified Data.Functor.Barbie.Layered          as B
import           Data.Functor.Identity                (Identity (Identity, runIdentity))
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
import Data.Functor.With (MayHave)
import qualified Data.Functor.With as W

type Module t et ev = S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev

type Definition t et ev = S.Definition 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev

type Type = S.Type 'S.NameResolved 'S.Curried

type Value t et ev = S.Value 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev

type ProcedureStep t et ev = S.ProcedureStep 'S.NameResolved 'S.Curried 'S.LambdaUnlifted t et ev

type VariableTypes f = Map S.QualifiedIdentifier (f (S.Type 'S.NameResolved 'S.Curried f))

typeCheck
  :: ( MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     , Eq (f (S.Type 'S.NameResolved 'S.Curried f))
     , Show (f (S.Type 'S.NameResolved 'S.Curried f))
     , Show (f S.QualifiedIdentifier)
     )
  => Map S.QualifiedIdentifier (f (S.Type 'S.NameResolved 'S.Curried f))
  -> f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed et ev f))
typeCheck = typeCheck'

typeCheck'
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => VariableTypes f
  -> f (Module 'S.Untyped et ev f)
  -> m (f (Module 'S.Typed et ev f))
typeCheck' variableTypes m =
  traverse go m
  where
    prim = primitivesImported m
    go (S.Module mn ms ds) = S.Module mn ms <$> definitions variableTypes prim ds

{-# ANN definitions ("HLint: ignore Use list literal" :: String) #-}

definitions
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => VariableTypes f
  -> PrimitivesImported
  -> f [f (Definition 'S.Untyped et ev f)]
  -> m (f [f (Definition 'S.Typed et ev f)])
definitions variableTypes prim =
  traverse go
  where
    go definitions' = do
      let
        valueBinds = M.fromList $ mapMaybe valueBind definitions'
        foreignValueBinds = M.fromList $ mapMaybe foreignValueBind definitions'
        dependencyGraph = G.overlays $ dependency (M.keysSet valueBinds) <$> definitions'
        sortedIdentifiers = fromRight unreachable $ G.topSort $ G.scc dependencyGraph
        variableTypes' = M.unions $ M.mapMaybe annotatedType valueBinds : foreignValueBinds : variableTypes : [] -- why not a list literal? because of non-injective type families.
        typedValueBinds = M.unions $ mapMaybe valueConstructors definitions'
      typedValueBinds <- foldr (flip $ typeBind variableTypes' valueBinds prim) (pure typedValueBinds) sortedIdentifiers
      pure $ replaceValue typedValueBinds <$> definitions'

dependency :: (Copointed f, MayHave S.Location f) => Set S.QualifiedIdentifier -> f (Definition 'S.Untyped et ev f) -> G.AdjacencyMap S.QualifiedIdentifier
dependency valueBinds d
  | S.ValueBind b <- copoint d =
      let S.ValueBindU i v = copoint b
      in G.vertex (copoint i) `G.connect` G.overlays (G.vertex <$> dep valueBinds v)
dependency _ _ = G.empty

typeBind
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => VariableTypes f
  -> Map S.QualifiedIdentifier (f (Value 'S.Untyped et ev f))
  -> PrimitivesImported
  -> m (Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)))
  -> GN.AdjacencyMap S.QualifiedIdentifier
  -> m (Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)))
typeBind variableTypes valueBinds prim typedValueBinds recursionIdentifiers =
  catchJust
    (\case { NotFoundException i _ -> if GN.hasVertex i recursionIdentifiers then Just i else Nothing; _ -> Nothing })
    do
      typedValueBinds' <- typedValueBinds
      let
        variableTypes' = ((\v -> let S.TypedValue _ t = copoint v in t) <$> typedValueBinds') `M.union` variableTypes
        recursionValueBinds = M.filterWithKey (const . flip GN.hasVertex recursionIdentifiers) valueBinds
      recursionTypedValueBinds <- mapM (typeOfTerm variableTypes' prim) recursionValueBinds
      pure $ recursionTypedValueBinds `M.union` typedValueBinds'
    $ const $ throw $ RecursionException $ S.fromList $ N.toList $ GN.vertexList1 recursionIdentifiers

annotatedType :: Copointed f => f (Value 'S.Untyped et ev f) -> Maybe (f (Type f))
annotatedType v
  | S.UntypedValue v <- copoint v
  , S.TypeAnnotation a <- copoint v
  , S.TypeAnnotation' _ t <- copoint a = Just t
annotatedType _ = Nothing

replaceValue :: (Functor f, Copointed f) => Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)) -> f (Definition 'S.Untyped et ev f) -> f (Definition 'S.Typed et ev f)
replaceValue typedValueBinds =
  fmap $ \case
    S.DataDefinition i cs -> S.DataDefinition i cs
    S.TypeBind i t -> S.TypeBind i t
    S.ForeignTypeBind i c -> S.ForeignTypeBind i c
    S.ValueBind b -> S.ValueBind $ (\(S.ValueBindU i _) -> S.ValueBindU i $ fromMaybe unreachable $ M.lookup (copoint i) typedValueBinds) <$> b
    S.ForeignValueBind i c t -> S.ForeignValueBind i c t

valueBind :: Copointed f => f (Definition t et ev f) -> Maybe (S.QualifiedIdentifier, f (Value t et ev f))
valueBind d
  | S.ValueBind b <- copoint d =
      let S.ValueBindU i v = copoint b
      in Just (copoint i, v)
valueBind _ = Nothing

foreignValueBind :: Copointed f => f (Definition t et ev f) -> Maybe (S.QualifiedIdentifier, f (Type f))
foreignValueBind d | S.ForeignValueBind i _ t <- copoint d       = Just (copoint i, t)
foreignValueBind _                                               = Nothing

valueConstructors :: (Functor f, Copointed f) => f (Definition 'S.Untyped et ev f) -> Maybe (Map S.QualifiedIdentifier (f (Value 'S.Typed et ev f)))
valueConstructors d
  | S.DataDefinition i r <- copoint d =
      let
        dataRepresentation r =
          case copoint r of
            S.ValueConstructorsData cs ->
              M.fromList $ flip fmap (copoint cs) $ \c ->
                let
                  S.ValueConstructor j fs = copoint c
                  go f r =
                    let S.Field _ t = copoint f
                    in S.FunctionType (S.FunctionTypeC t r <$ f) <$ f
                in (copoint j, S.TypedValue (S.Variable j <$ j) (foldr go (S.TypeVariable i <$ i) $ copoint fs) <$ r)
            S.ForAllDataC a r' -> fmap (\(S.TypedValue v t) -> S.TypedValue v $ S.ForAllType a t <$ r) <$> dataRepresentation r'
      in Just $ dataRepresentation r
valueConstructors _ = Nothing

dep :: (Copointed f, MayHave S.Location f) => Set S.QualifiedIdentifier -> f (Value 'S.Untyped et ev f) -> [S.QualifiedIdentifier]
dep identifiers v =
  case copoint v of
    S.UntypedValue v' ->
      case copoint v' of
        S.Variable i
          | i' <- copoint i
          , i' `S.member` identifiers -> [i']
          | otherwise -> []
        S.Function f
          | S.FunctionC i _ v' <- copoint f -> dep (S.delete (copoint i) identifiers) v'
        S.Literal _ -> []
        S.Application a
          | S.ApplicationC v1 v2 <- copoint a -> mconcat $ dep identifiers <$> [v1, v2]
        S.Procedure ps ->
          fst $ foldl' go ([], identifiers) $ copoint ps
          where
            go (is, ss) s =
              case copoint s of
                S.BindProcedureStep i v' -> (dep ss v' ++ is, S.delete (copoint i) ss)
                S.CallProcedureStep v'   -> (dep ss v' ++ is, ss)
        S.TypeAnnotation a
          | S.TypeAnnotation' v' _ <- copoint a -> dep identifiers v'
        S.Let ds v ->
          let
            valueBinds = M.fromList $ mapMaybe valueBind $ copoint ds
            identifiers' = identifiers S.\\ M.keysSet valueBinds
          in dep identifiers' =<< v : M.elems valueBinds
        S.ForAllValue _ v -> dep identifiers v
        S.Instantiation i ->
          let S.InstantiationC v _ = copoint i
          in dep identifiers v

typeOfTerm
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => VariableTypes f
  -> PrimitivesImported
  -> f (Value 'S.Untyped et ev f)
  -> m (f (Value 'S.Typed et ev f))
typeOfTerm variableTypes prim@PrimitivesImported { int = primInt, frac2 = primFrac2, string = primString } v =
  case copoint v of
    S.UntypedValue v' ->
      case copoint v' of
        S.Variable i ->
          let i' = copoint i
          in
            case M.lookup i' variableTypes of
              Nothing -> throw $ NotFoundException i' $ W.mayGet i
              Just t  -> pure $ S.TypedValue (S.Variable i <$ v') t <$ v
        S.Literal l ->
          case copoint l of
            S.Integer v_ b ->
              let int = S.GlobalIdentifier ["kmkm", "prim"] "int"
              in
                if primInt
                  then pure $ S.TypedValue (S.Literal (S.Integer v_ b <$ l) <$ v') (S.TypeVariable (int <$ v) <$ v) <$ v
                  else throw $ PrimitiveTypeException int $ W.mayGet v
            S.Fraction s d e b ->
              let frac2 = S.GlobalIdentifier ["kmkm", "prim"] "frac2"
              in
                if primFrac2
                  then pure $ S.TypedValue (S.Literal (S.Fraction s d e b <$ l) <$ v') (S.TypeVariable (frac2 <$ v) <$ v) <$ v
                  else throw $ PrimitiveTypeException frac2 $ W.mayGet v
            S.String t ->
              let string = S.GlobalIdentifier ["kmkm", "prim"] "string"
              in
                if primString
                  then pure $ S.TypedValue (S.Literal (S.String t <$ l) <$ v') (S.TypeVariable (string <$ v) <$ v) <$ v
                  else throw $ PrimitiveTypeException string $ W.mayGet v
        S.Function f
          | S.FunctionC i t v'' <- copoint f -> do
              v''' <- typeOfTerm (M.insert (copoint i) t variableTypes) prim v''
              let S.TypedValue _ t' = copoint v'''
              pure $ S.TypedValue (S.Function (S.FunctionC i t v''' <$ f) <$ v') (S.FunctionType (S.FunctionTypeC t t' <$ v) <$ v) <$ v
        S.Application a
          | S.ApplicationC v0 v1 <- copoint a -> do
              v0' <- typeOfTerm variableTypes prim v0
              v1' <- typeOfTerm variableTypes prim v1
              let
                S.TypedValue _ t0 = copoint v0'
                S.TypedValue _ t1 = copoint v1'
              case copoint t0 of
                S.FunctionType t ->
                  case copoint t of
                    S.FunctionTypeC t00 t01
                      | S.toIdentity t1 == S.toIdentity t00 -> pure $ S.TypedValue (S.Application (S.ApplicationC v0' v1' <$ a) <$ v') t01 <$ v
                      | otherwise                           -> throw $ MismatchException (Right $ S.toIdentity t00) (S.toIdentity t1) $ W.mayGet v1'
                _ -> throw $ MismatchException (Left "function") (S.toIdentity t0) $ W.mayGet v0'
        S.Procedure ps -> do
          let p :| ps' = copoint ps
          (variableTypes', p') <- typeOfProcedureStep variableTypes prim p
          (_, ps'') <- foldr go (pure (variableTypes', [])) ps'
          let ps''' = p' :| ps''
          case N.last ps''' of
            s
              | S.CallProcedureStep v <- copoint s ->
                  let S.TypedValue _ t = copoint v
                  in pure $ S.TypedValue (S.Procedure (ps''' <$ ps) <$ v') t <$ v
              | otherwise -> throw $ BindProcedureEndException $ W.mayGet s
          where
            go p acc = do
              (variableTypes, ps) <- acc
              (variableTypes', p') <- typeOfProcedureStep variableTypes prim p
              pure (variableTypes', p' : ps)
        S.TypeAnnotation a
          | S.TypeAnnotation' v' t <- copoint a -> do
              v'' <- typeOfTerm variableTypes prim v'
              let S.TypedValue _ t' = copoint v''
              if S.toIdentity t == S.toIdentity t'
                then pure v''
                else throw $ MismatchException (Right $ S.toIdentity t) (S.toIdentity t') $ W.mayGet v''
        S.Let ds v' -> do
          ds' <- definitions variableTypes prim ds
          let variableTypes' = ((\(S.TypedValue _ t) -> t) . copoint <$> M.fromList (mapMaybe valueBind $ copoint ds')) `M.union` variableTypes
          v'' <- typeOfTerm variableTypes' prim v'
          let S.TypedValue _ t' = copoint v''
          pure $ S.TypedValue (S.Let ds' v'' <$ v') t' <$ v
        S.ForAllValue i v' -> do
          v'' <- typeOfTerm variableTypes prim v'
          let S.TypedValue _ t = copoint v''
          pure $ S.TypedValue (S.ForAllValue i v'' <$ v) (S.ForAllType i t <$ v) <$ v
        S.Instantiation i -> do
          let S.InstantiationC v0 t1 = copoint i
          v0' <- typeOfTerm variableTypes prim v0
          let S.TypedValue _ t0 = copoint v0'
          case copoint t0 of
            S.ForAllType j t01 -> do
              let t01' = substitute j t1 t01
              pure $ S.TypedValue (S.Instantiation (S.InstantiationC v0' t1 <$ i) <$ v) t01' <$ v
            _ -> throw $ MismatchException (Left "for-all value") (S.toIdentity t0) $ W.mayGet v0'

substitute :: (Functor f, Copointed f) => f S.QualifiedIdentifier -> f (Type f) -> f (Type f) -> f (Type f)
substitute i value target =
  case copoint target of
    S.TypeVariable i'
      | copoint i == copoint i' -> value
      | otherwise -> target
    S.TypeApplication t0 t1 -> S.TypeApplication (substitute i value t0) (substitute i value t1) <$ target
    S.FunctionType t | S.FunctionTypeC t0 t1 <- copoint t -> S.FunctionType (S.FunctionTypeC (substitute i value t0) (substitute i value t1) <$ t) <$ target
    S.ProcedureType t0 -> S.ProcedureType (substitute i value t0) <$ target
    S.ForAllType i' t
      | copoint i == copoint i' -> target
      | otherwise -> S.ForAllType i' (substitute i value t) <$ target

typeOfProcedureStep
  :: ( MonadThrow m
     , MonadCatch m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , Eq (f (Type f))
     , Show (f (Type f))
     , Show (f S.QualifiedIdentifier)
     )
  => VariableTypes f
  -> PrimitivesImported
  -> f (ProcedureStep 'S.Untyped et ev f)
  -> m (VariableTypes f, f (ProcedureStep 'S.Typed et ev f))
typeOfProcedureStep variableTypes prim s =
  case copoint s of
    S.BindProcedureStep i v -> do
      v' <- typeOfTerm variableTypes prim v
      let
        S.TypedValue _ t = copoint v'
        variableTypes' = M.insert (copoint i) t variableTypes
      pure (variableTypes', S.BindProcedureStep i v' <$ s)
    S.CallProcedureStep v -> do
      v' <- typeOfTerm variableTypes prim v
      let S.TypedValue _ t = copoint v'
      case copoint t of
        S.ProcedureType _ -> pure (variableTypes, S.CallProcedureStep v' <$ s)
        _                 -> throw $ MismatchException (Left "procedure") (S.toIdentity t) $ W.mayGet t

primitivesImported :: (Functor f, Copointed f, B.FunctorB et, B.FunctorB ev) => f (Module 'S.Untyped et ev f) -> PrimitivesImported
primitivesImported m =
  case S.toIdentity m of
    Identity (S.Module _ ms _) ->
      if Identity ["kmkm", "prim"] `elem` runIdentity ms
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
  | MismatchException { expected :: Either Text (Identity (S.Type 'S.NameResolved 'S.Curried Identity)), actual :: Identity (S.Type 'S.NameResolved 'S.Curried Identity), location :: Maybe S.Location }
  | BindProcedureEndException (Maybe S.Location)
  | RecursionException (Set S.QualifiedIdentifier)
  | PrimitiveTypeException S.QualifiedIdentifier (Maybe S.Location)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
