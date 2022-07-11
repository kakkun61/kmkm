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

import qualified Barbies.Bare                     as B (Covered)
import qualified Barbies.Bare.Layered             as B (BareB)
import qualified Control.Exception                as E
import           Control.Exception.Safe           (MonadThrow, throw)
import           Data.Copointed                   (Copointed (copoint))
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Traversable                 (for)
import qualified Data.Typeable                    as Y
import           GHC.Generics                     (Generic)
import qualified Language.Kmkm.Internal.Exception as X

type Module n et ev f = S.Module n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type Definition n et ev f = S.Definition n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type ValueBind n et ev f = S.ValueBind n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type Value n et ev f = S.Value n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type Value' n et ev f = S.Value' n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type ProcedureStep n et ev f = S.ProcedureStep n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type TypeAnnotation n et ev f = S.TypeAnnotation n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type Application n et ev f = S.Application n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type Function n et ev f = S.Function n 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f

type Type n f = S.Type n 'S.Curried B.Covered f

type FunctionType n f = S.FunctionType n 'S.Curried B.Covered f

nameResolve
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
     )
  => Map S.ModuleName (Set S.Identifier)
  -> Map S.ModuleName (Set S.Identifier)
  -> f (S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered f))
nameResolve = module'

module'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
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
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
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
    S.DataDefinition i cs ->
      S.DataDefinition (S.GlobalIdentifier moduleName <$> i) <$> mapM (traverse constructor) cs
      where
        constructor =
          traverse $ \(i, fs) -> do
            fs' <- mapM (traverse field) fs
            pure (S.GlobalIdentifier moduleName <$> i, fs')
        field =
          traverse $ \(i, t) -> do
            t' <- typ typeAffiliations moduleName t
            pure (S.GlobalIdentifier moduleName <$> i, t')
    S.ForeignValueBind i c t -> S.ForeignValueBind (S.GlobalIdentifier moduleName <$> i) c <$> typ typeAffiliations moduleName t

valueBind
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Affiliation
  -> ValueBind 'S.NameUnresolved et ev f
  -> m (ValueBind 'S.NameResolved et ev f)
valueBind valueAffiliations typeAffiliations moduleName (Global _) (S.ValueBindU i v) =
  S.ValueBindU (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v
valueBind valueAffiliations typeAffiliations moduleName Local (S.ValueBindU i v) =
  S.ValueBindU (S.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations moduleName v

value
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Value 'S.NameUnresolved et ev f)
  -> m (f (Value 'S.NameResolved et ev f))
value valueAffiliations typeAffiliations moduleName v =
  traverse go v
  where
    go (S.UntypedValue v) = S.UntypedValue <$> value' valueAffiliations typeAffiliations moduleName v

value'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Value' 'S.NameUnresolved et ev f)
  -> m (f (Value' 'S.NameResolved et ev f))
value' valueAffiliations typeAffiliations moduleName v =
  for v $ \case
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
        valueAffiliations' = localAffiliations `M.union` valueAffiliations
      in
        S.Let
          <$> mapM (traverse $ definition valueAffiliations' typeAffiliations moduleName Local) ds
          <*> value valueAffiliations' typeAffiliations moduleName v
    S.ForAll i v ->
      let typeAffiliations' = M.insert (copoint i) Local typeAffiliations
      in S.ForAll (S.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations' moduleName v

procedureStep
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
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
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
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
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
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
     , S.HasLocation f
     , B.BareB et
     , B.BareB ev
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

typ
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasLocation f
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
     , S.HasLocation f
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
     , S.HasLocation f
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
        Nothing         -> throw $ UnknownIdentifierException (copoint i) $ S.location i
    S.QualifiedIdentifier i'@(S.GlobalIdentifier n i'') ->
      case M.lookup i'' affiliations of
        Just (Global n') | n == n' -> pure $ i' <$ i
        _                          -> throw $ UnknownIdentifierException (copoint i) $ S.location i
    S.QualifiedIdentifier i' -> pure $ i' <$ i

boundIdentifiers :: (Functor f, Functor g, Copointed g, B.BareB et, B.BareB ev) => f (g (Module 'S.NameUnresolved et ev g)) -> (f (Set S.Identifier), f (Set S.Identifier))
boundIdentifiers modules =
  (eachModule boundValueIdentifier <$> modules, eachModule boundTypeIdentifier <$> modules)
  where
    eachModule f m = let S.Module _ _ ds = copoint m in S.unions $ f <$> copoint ds

boundValueIdentifier :: (Functor f, Copointed f, B.BareB et, B.BareB ev) => f (Definition 'S.NameUnresolved et ev f) -> Set S.Identifier
boundValueIdentifier =
  boundValueIdentifier' . S.strip
  where
    boundValueIdentifier' (S.DataDefinition _ cs)          = S.fromList $ fst <$> cs
    boundValueIdentifier' (S.ValueBind (S.ValueBindU i _)) = S.singleton i
    boundValueIdentifier' (S.ForeignValueBind i _ _)       = S.singleton i
    boundValueIdentifier' _                                = S.empty

boundTypeIdentifier :: (Functor f, Copointed f, B.BareB et, B.BareB ev) => f (Definition 'S.NameUnresolved et ev f) -> Set S.Identifier
boundTypeIdentifier =
  boundTypeIdentifier' . S.strip
  where
    boundTypeIdentifier' (S.DataDefinition i _)  = S.singleton i
    boundTypeIdentifier' (S.TypeBind i _)        = S.singleton i
    boundTypeIdentifier' (S.ForeignTypeBind i _) = S.singleton i
    boundTypeIdentifier' _                       = S.empty

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
