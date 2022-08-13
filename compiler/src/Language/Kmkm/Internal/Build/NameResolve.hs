{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Kmkm.Internal.Build.NameResolve
  ( nameResolve
  , boundIdentifiers
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Syntax as S

import qualified Control.Exception                as E
import           Control.Exception.Safe           (MonadThrow, throw)
import           Data.Copointed                   (Copointed (copoint))
import qualified Data.Functor.Barbie.Layered      as B
import           Data.Functor.Identity            (Identity (Identity, runIdentity))
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import qualified Data.Typeable                    as Y
import           GHC.Generics                     (Generic)
import qualified Language.Kmkm.Internal.Exception as X
import qualified Data.Functor.With as W
import Data.Functor.With (MayHave)

type Module n et ev = S.Module n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Definition n et ev = S.Definition n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type DataRepresentation n et ev = S.DataRepresentation n 'S.Curried 'S.LambdaUnlifted et ev

type ValueConstructor n et ev = S.ValueConstructor n 'S.Curried 'S.LambdaUnlifted et ev

type Field n et ev = S.Field n 'S.Curried 'S.LambdaUnlifted et ev

type ValueBind n et ev = S.ValueBind n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Value n et ev = S.Value n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Value' n et ev = S.Value' n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type ProcedureStep n et ev = S.ProcedureStep n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type TypeAnnotation n et ev = S.TypeAnnotation n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Application n et ev = S.Application n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Function n et ev = S.Function n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Instantiation n et ev = S.Instantiation n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev

type Type n = S.Type n 'S.Curried

type FunctionType n = S.FunctionType n 'S.Curried

nameResolve
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.ModuleName (Set S.Identifier)
  -> Map S.ModuleName (Set S.Identifier)
  -> f (S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev f))
nameResolve = module'

module'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.ModuleName (Set S.Identifier)
  -> Map S.ModuleName (Set S.Identifier)
  -> f (Module 'S.NameUnresolved et ev f)
  -> m (f (Module 'S.NameResolved et ev f))
module' valueIdentifiers typeIdentifiers =
  traverse $ \(S.Module moduleName ms ds) -> do
    let
      moduleName' = copoint moduleName
      ms' = moduleName' : (copoint <$> copoint ms)
      valueIdentifiers' = M.insert moduleName' (S.unions $ boundValueIdentifier <$> copoint ds) valueIdentifiers
      valueAffiliations = importedAffiliations valueIdentifiers' $ S.fromList ms'
      typeIdentifiers' = M.insert moduleName' (S.unions $ boundTypeIdentifier <$> copoint ds) typeIdentifiers
      typeAffiliations = importedAffiliations typeIdentifiers' $ S.fromList ms'
    S.Module moduleName ms <$> mapM (traverse $ definition valueAffiliations typeAffiliations moduleName' $ Global moduleName') ds

definition
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Affiliation
  -> f (Definition 'S.NameUnresolved et ev f)
  -> m (f (Definition 'S.NameResolved et ev f))
definition valueAffiliations typeAffiliations moduleName affiliation =
  traverse $ \case
    S.ValueBind b -> S.ValueBind <$> valueBind valueAffiliations typeAffiliations moduleName affiliation b
    S.TypeBind i t -> S.TypeBind (S.GlobalIdentifier moduleName <$> i) <$> typ typeAffiliations moduleName t
    S.ForeignTypeBind i c -> pure $ S.ForeignTypeBind (S.GlobalIdentifier moduleName <$> i) c
    S.DataDefinition i r -> S.DataDefinition (S.GlobalIdentifier moduleName <$> i) <$> dataRepresentation typeAffiliations moduleName r
    S.ForeignValueBind i c t -> S.ForeignValueBind (S.GlobalIdentifier moduleName <$> i) c <$> typ typeAffiliations moduleName t

dataRepresentation :: (MonadThrow m, Traversable f, Copointed f, MayHave S.Location f) => Map S.Identifier Affiliation -> S.ModuleName -> f (DataRepresentation 'S.NameUnresolved et ev f) -> m (f (DataRepresentation 'S.NameResolved et ev f))
dataRepresentation typeAffiliations moduleName =
  traverse $ \case
    S.ForAllDataC i r -> S.ForAllDataC (S.LocalIdentifier <$> i) <$> dataRepresentation (M.insert (copoint i) Local typeAffiliations) moduleName r
    S.ValueConstructorsData cs -> S.ValueConstructorsData <$> mapM (traverse $ valueConstructor typeAffiliations moduleName) cs

valueConstructor :: (MonadThrow m, Traversable f, Copointed f, MayHave S.Location f) => Map S.Identifier Affiliation -> S.ModuleName -> f (ValueConstructor 'S.NameUnresolved et ev f) -> m (f (ValueConstructor 'S.NameResolved et ev f))
valueConstructor typeAffiliations moduleName =
  traverse $ \(S.ValueConstructor i fs) -> do
    fs' <- mapM (traverse $ field typeAffiliations moduleName) fs
    pure $ S.ValueConstructor (S.GlobalIdentifier moduleName <$> i) fs'

field :: (MonadThrow m, Traversable f, Copointed f, MayHave S.Location f) => Map S.Identifier Affiliation -> S.ModuleName -> f (Field 'S.NameUnresolved et ev f) -> m (f (Field 'S.NameResolved et ev f))
field typeAffiliations moduleName =
  traverse $ \(S.Field i t) -> do
    t' <- typ typeAffiliations moduleName t
    pure $ S.Field (S.GlobalIdentifier moduleName <$> i) t'

valueBind
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Affiliation
  -> f (ValueBind 'S.NameUnresolved et ev f)
  -> m (f (ValueBind 'S.NameResolved et ev f))
valueBind valueAffiliations typeAffiliations moduleName (Global _) =
  traverse $ \(S.ValueBindU i v) ->
    S.ValueBindU (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v
valueBind valueAffiliations typeAffiliations moduleName Local =
  traverse $ \(S.ValueBindU i v) ->
    S.ValueBindU (S.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations moduleName v

value
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Value 'S.NameUnresolved et ev f)
  -> m (f (Value 'S.NameResolved et ev f))
value valueAffiliations typeAffiliations moduleName =
  traverse $ \(S.UntypedValue v) -> S.UntypedValue <$> value' valueAffiliations typeAffiliations moduleName v

value'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Value' 'S.NameUnresolved et ev f)
  -> m (f (Value' 'S.NameResolved et ev f))
value' valueAffiliations typeAffiliations moduleName =
  traverse $ \case
    S.Variable i -> S.Variable <$> referenceIdentifier valueAffiliations i
    S.Procedure ss -> S.Procedure <$> mapM (traverse $ procedureStep valueAffiliations typeAffiliations moduleName) ss
    S.Literal l -> pure $ S.Literal $ literal <$> l
    S.Function f -> S.Function <$> function valueAffiliations typeAffiliations moduleName f
    S.TypeAnnotation a -> S.TypeAnnotation <$> typeAnnotation valueAffiliations typeAffiliations moduleName a
    S.Application a -> S.Application <$> application valueAffiliations typeAffiliations moduleName a
    S.Let ds v ->
      let
        localIdentifiers = S.unions $ boundValueIdentifier <$> copoint ds
        localAffiliations = M.fromSet (const Local) localIdentifiers
        valueAffiliations' = M.union localAffiliations valueAffiliations
      in
        S.Let
          <$> mapM (traverse $ definition valueAffiliations' typeAffiliations moduleName Local) ds
          <*> value valueAffiliations' typeAffiliations moduleName v
    S.ForAllValue i v ->
      let typeAffiliations' = M.insert (copoint i) Local typeAffiliations
      in S.ForAllValue (S.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations' moduleName v
    S.Instantiation i -> S.Instantiation <$> instantiation valueAffiliations typeAffiliations moduleName i

procedureStep
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (ProcedureStep 'S.NameUnresolved et ev f)
  -> m (f (ProcedureStep 'S.NameResolved et ev f))
procedureStep valueAffiliations typeAffiliations moduleName =
  traverse $ \case
    S.CallProcedureStep v   -> S.CallProcedureStep <$> value valueAffiliations typeAffiliations moduleName v
    S.BindProcedureStep i v -> S.BindProcedureStep (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v

literal :: S.Literal -> S.Literal
literal (S.Integer v b)      = S.Integer v b
literal (S.Fraction f s e b) = S.Fraction f s e b
literal (S.String s)         = S.String s

typeAnnotation
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (TypeAnnotation 'S.NameUnresolved et ev f)
  -> m (f (TypeAnnotation 'S.NameResolved et ev f))
typeAnnotation valueAffiliations typeAffiliations moduleName =
  traverse $ \(S.TypeAnnotation' v t) ->
    S.TypeAnnotation' <$> value valueAffiliations typeAffiliations moduleName v <*> typ typeAffiliations moduleName t

application
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Application 'S.NameUnresolved et ev f)
  -> m (f (Application 'S.NameResolved et ev f))
application valueAffiliations typeAffiliations moduleName =
  traverse $ \(S.ApplicationC v1 v2) ->
    S.ApplicationC <$> value valueAffiliations typeAffiliations moduleName v1 <*> value valueAffiliations typeAffiliations moduleName v2

function
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Function 'S.NameUnresolved et ev f)
  -> m (f (Function 'S.NameResolved et ev f))
function valueAffiliations typeAffiliations moduleName =
  traverse $ \(S.FunctionC i t v) ->
    let valueAffiliations' = M.insert (copoint i) Local valueAffiliations
    in S.FunctionC (S.LocalIdentifier <$> i) <$> typ typeAffiliations moduleName t <*> value valueAffiliations' typeAffiliations moduleName v

instantiation
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Instantiation 'S.NameUnresolved et ev f)
  -> m (f (Instantiation 'S.NameResolved et ev f))
instantiation valueAffiliations typeAffiliations moduleName =
  traverse $ \(S.InstantiationC v t) ->
    S.InstantiationC <$> value valueAffiliations typeAffiliations moduleName v <*> typ typeAffiliations moduleName t

typ
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     )
  => Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Type 'S.NameUnresolved f)
  -> m (f (Type 'S.NameResolved f))
typ typeAffiliations moduleName =
  traverse $ \case
    S.TypeVariable i  -> S.TypeVariable <$> referenceIdentifier typeAffiliations i
    S.ProcedureType t -> S.ProcedureType <$> typ typeAffiliations moduleName t
    S.TypeApplication t1 t2 -> S.TypeApplication <$> typ typeAffiliations moduleName t1 <*> typ typeAffiliations moduleName t2
    S.FunctionType t -> S.FunctionType <$> functionType typeAffiliations moduleName t
    S.ForAllType i t -> do
      let typeAffiliations' = M.insert (copoint i) Local typeAffiliations
      S.ForAllType (S.LocalIdentifier <$> i) <$> typ typeAffiliations' moduleName t

functionType
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave S.Location f
     )
  => Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (FunctionType 'S.NameUnresolved f)
  -> m (f (FunctionType 'S.NameResolved f))
functionType typeAffiliations moduleName =
  traverse $ \(S.FunctionTypeC t1 t2) ->
    S.FunctionTypeC <$> typ typeAffiliations moduleName t1 <*> typ typeAffiliations moduleName t2

referenceIdentifier
  :: ( MonadThrow m
     , Functor f
     , Copointed f
     , MayHave S.Location f
     )
  => Map S.Identifier Affiliation
  -> f (S.ReferenceIdentifier 'S.NameUnresolved)
  -> m (f (S.ReferenceIdentifier 'S.NameResolved))
referenceIdentifier affiliations i =
  case copoint i of
    S.UnqualifiedIdentifier i' ->
      case M.lookup i' affiliations of
        Just (Global n) -> pure $ S.GlobalIdentifier n i' <$ i
        Just Local      -> pure $ S.LocalIdentifier i' <$ i
        Nothing         -> throw $ UnknownIdentifierException (copoint i) $ W.mayGet i
    S.QualifiedIdentifier i'@(S.GlobalIdentifier n i'') ->
      case M.lookup i'' affiliations of
        Just (Global n') | n == n' -> pure $ i' <$ i
        _                          -> throw $ UnknownIdentifierException (copoint i) $ W.mayGet i
    S.QualifiedIdentifier i' -> pure $ i' <$ i

boundIdentifiers :: (Functor f, Functor g, Copointed g, B.FunctorB et, B.FunctorB ev) => f (g (Module 'S.NameUnresolved et ev g)) -> (f (Set S.Identifier), f (Set S.Identifier))
boundIdentifiers modules =
  (eachModule boundValueIdentifier <$> modules, eachModule boundTypeIdentifier <$> modules)
  where
    eachModule f m = let S.Module _ _ ds = copoint m in S.unions $ f <$> copoint ds

boundValueIdentifier :: (Functor f, Copointed f, B.FunctorB et, B.FunctorB ev) => f (Definition 'S.NameUnresolved et ev f) -> Set S.Identifier
boundValueIdentifier =
  boundValueIdentifier' . runIdentity . S.toIdentity
  where
    boundValueIdentifier' (S.DataDefinition _ (Identity r))                      = identifiers r
    boundValueIdentifier' (S.ValueBind (Identity (S.ValueBindU (Identity i) _))) = S.singleton i
    boundValueIdentifier' (S.ForeignValueBind (Identity i) _ _)                  = S.singleton i
    boundValueIdentifier' _                                                      = S.empty
    identifiers (S.ForAllDataC _ (Identity r)) = identifiers r
    identifiers (S.ValueConstructorsData (Identity cs))               = S.fromList $ (\(Identity (S.ValueConstructor (Identity i) _)) -> i) <$> cs

boundTypeIdentifier :: (Functor f, Copointed f, B.FunctorB et, B.FunctorB ev) => f (Definition 'S.NameUnresolved et ev f) -> Set S.Identifier
boundTypeIdentifier =
  boundTypeIdentifier' . runIdentity . S.toIdentity
  where
    boundTypeIdentifier' (S.DataDefinition (Identity i) _)  = S.singleton i
    boundTypeIdentifier' (S.TypeBind (Identity i) _)        = S.singleton i
    boundTypeIdentifier' (S.ForeignTypeBind (Identity i) _) = S.singleton i
    boundTypeIdentifier' _                                  = S.empty

importedAffiliations :: Map S.ModuleName (Set S.Identifier) -> Set S.ModuleName -> Map S.Identifier Affiliation
importedAffiliations identifiers importedModuleNames =
  M.unions $ S.map (\n -> M.fromSet (const $ Global n) $ identifiers M.! n) importedModuleNames

data Affiliation
  = Global S.ModuleName
  | Local
  deriving (Show, Read, Eq, Ord, Generic)

data Exception
  = UnknownIdentifierException S.EitherIdentifier (Maybe S.Location)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
