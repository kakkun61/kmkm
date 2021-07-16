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

import qualified Barbies.Bare                     as B
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

type Module n f = S.Module n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type Definition n f = S.Definition n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type ValueBind n f = S.ValueBind n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type Value n f = S.Value n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type Value' n f = S.Value' n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type ProcedureStep n f = S.ProcedureStep n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type TypeAnnotation n f = S.TypeAnnotation n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type Application n f = S.Application n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type Function n f = S.Function n 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f

type Type n f = S.Type n 'S.Curried B.Covered f

type FunctionType n f = S.FunctionType n 'S.Curried B.Covered f

nameResolve
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.ModuleName (Set S.Identifier)
  -> Map S.ModuleName (Set S.Identifier)
  -> f (S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped B.Covered f))
nameResolve = module'

module'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.ModuleName (Set S.Identifier)
  -> Map S.ModuleName (Set S.Identifier)
  -> f (Module 'S.NameUnresolved f)
  -> m (f (Module 'S.NameResolved f))
module' valueIdentifiers typeIdentifiers =
  traverse $ \(S.Module moduleName ms ds) -> do
    let
      moduleName' = copoint moduleName
      ms' = moduleName' : (copoint <$> copoint ms)
      valueIdentifiers' = M.insert moduleName' (S.unions $ boundValueIdentifier <$> copoint ds) valueIdentifiers
      valueAffiliations = importedAffiliations valueIdentifiers' $ S.fromList ms'
      typeIdentifiers' = M.insert moduleName' (S.unions $ boundTypeIdentifier <$> copoint ds) typeIdentifiers
      typeAffiliations = importedAffiliations typeIdentifiers' $ S.fromList ms'
    S.Module moduleName ms <$> sequence (traverse (definition valueAffiliations typeAffiliations moduleName' $ Global moduleName') <$> ds)

definition
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Affiliation
  -> f (Definition 'S.NameUnresolved f)
  -> m (f (Definition 'S.NameResolved f))
definition valueAffiliations typeAffiliations moduleName affiliation =
  traverse $ \case
    S.ValueBind b -> S.ValueBind <$> valueBind valueAffiliations typeAffiliations moduleName affiliation b
    S.TypeBind i t -> S.TypeBind (S.GlobalIdentifier moduleName <$> i) <$> typ typeAffiliations moduleName t
    S.ForeignTypeBind i hs c -> pure $ S.ForeignTypeBind (S.GlobalIdentifier moduleName <$> i) hs c
    S.DataDefinition i cs ->
      S.DataDefinition (S.GlobalIdentifier moduleName <$> i) <$> sequence (traverse constructor <$> cs)
      where
        constructor =
          traverse $ \(i, fs) -> do
            fs' <- sequence (traverse field <$> fs)
            pure (S.GlobalIdentifier moduleName <$> i, fs')
        field =
          traverse $ \(i, t) -> do
            t' <- typ typeAffiliations moduleName t
            pure (S.GlobalIdentifier moduleName <$> i, t')
    S.ForeignValueBind i hs c t -> S.ForeignValueBind (S.GlobalIdentifier moduleName <$> i) hs c <$> typ typeAffiliations moduleName t

valueBind
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Affiliation
  -> ValueBind 'S.NameUnresolved f
  -> m (ValueBind 'S.NameResolved f)
valueBind valueAffiliations typeAffiliations moduleName (Global _) (S.ValueBindU i v) =
  S.ValueBindU (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v
valueBind valueAffiliations typeAffiliations moduleName Local (S.ValueBindU i v) =
  S.ValueBindU (S.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations moduleName v

value
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Value 'S.NameUnresolved f)
  -> m (f (Value 'S.NameResolved f))
value valueAffiliations typeAffiliations moduleName v =
  traverse go v
  where
    go (S.UntypedValue v) = S.UntypedValue <$> value' valueAffiliations typeAffiliations moduleName v

value'
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (Value' 'S.NameUnresolved f)
  -> m (f (Value' 'S.NameResolved f))
value' valueAffiliations typeAffiliations moduleName v =
  for v $ \case
    S.Variable i -> S.Variable <$> referenceIdentifier valueAffiliations i
    S.Procedure ss -> S.Procedure <$> sequence (traverse (procedureStep valueAffiliations typeAffiliations moduleName) <$> ss)
    S.Literal l -> pure $ S.Literal $ literal l
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
          <$> sequence (traverse (definition valueAffiliations' typeAffiliations moduleName Local) <$> ds)
          <*> value valueAffiliations' typeAffiliations moduleName v

procedureStep
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> f (ProcedureStep 'S.NameUnresolved f)
  -> m (f (ProcedureStep 'S.NameResolved f))
procedureStep valueAffiliations typeAffiliations moduleName =
  traverse $ \case
    S.TermProcedure v -> S.TermProcedure <$> value valueAffiliations typeAffiliations moduleName v
    S.BindProcedure i v -> S.BindProcedure (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v

literal :: S.Literal -> S.Literal
literal (S.Integer v b)      = S.Integer v b
literal (S.Fraction f s e b) = S.Fraction f s e b
literal (S.String s)         = S.String s

typeAnnotation
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> TypeAnnotation 'S.NameUnresolved f
  -> m (TypeAnnotation 'S.NameResolved f)
typeAnnotation valueAffiliations typeAffiliations moduleName (S.TypeAnnotation' v t) =
  S.TypeAnnotation' <$> value valueAffiliations typeAffiliations moduleName v <*> typ typeAffiliations moduleName t

application
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Application 'S.NameUnresolved f
  -> m (Application 'S.NameResolved f)
application valueAffiliations typeAffiliations moduleName (S.ApplicationC v1 v2) =
  S.ApplicationC <$> value valueAffiliations typeAffiliations moduleName v1 <*> value valueAffiliations typeAffiliations moduleName v2

function
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> Map S.Identifier Affiliation
  -> S.ModuleName
  -> Function 'S.NameUnresolved f
  -> m (Function 'S.NameResolved f)
function valueAffiliations typeAffiliations moduleName (S.FunctionC i t v) =
  let valueAffiliations' = M.insert (copoint i) Local valueAffiliations
  in S.FunctionC (S.LocalIdentifier <$> i) <$> typ typeAffiliations moduleName t <*> value valueAffiliations' typeAffiliations moduleName v

typ
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
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
    S.FunctionType f -> S.FunctionType <$> functionType typeAffiliations moduleName f

functionType
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , S.HasPosition f
     )
  => Map S.Identifier Affiliation
  -> S.ModuleName
  -> FunctionType 'S.NameUnresolved f
  -> m (FunctionType 'S.NameResolved f)
functionType typeAffiliations moduleName (S.FunctionTypeC t1 t2) =
  S.FunctionTypeC <$> typ typeAffiliations moduleName t1 <*> typ typeAffiliations moduleName t2

referenceIdentifier
  :: ( MonadThrow m
     , Functor f
     , Copointed f
     , S.HasPosition f
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
        Nothing         -> throw $ UnknownIdentifierException (copoint i) $ S.range i
    S.QualifiedIdentifier i'@(S.GlobalIdentifier n i'') ->
      case M.lookup i'' affiliations of
        Just (Global n') | n == n' -> pure $ i' <$ i
        _                          -> throw $ UnknownIdentifierException (copoint i) $ S.range i
    S.QualifiedIdentifier i' -> pure $ i' <$ i

boundIdentifiers :: (Functor f, Functor g, Copointed g) => f (g (Module 'S.NameUnresolved g)) -> (f (Set S.Identifier), f (Set S.Identifier))
boundIdentifiers modules =
  (eachModule boundValueIdentifier <$> modules, eachModule boundTypeIdentifier <$> modules)
  where
    eachModule f m = let S.Module _ _ ds = copoint m in S.unions $ f <$> copoint ds

boundValueIdentifier :: (Functor f, Copointed f) => f (Definition 'S.NameUnresolved f) -> Set S.Identifier
boundValueIdentifier =
  boundValueIdentifier' . S.strip
  where
    boundValueIdentifier' (S.DataDefinition _ cs)          = S.fromList $ fst <$> cs
    boundValueIdentifier' (S.ValueBind (S.ValueBindU i _)) = S.singleton i
    boundValueIdentifier' (S.ForeignValueBind i _ _ _)     = S.singleton i
    boundValueIdentifier' _                                = S.empty

boundTypeIdentifier :: (Functor f, Copointed f) => f (Definition 'S.NameUnresolved f) -> Set S.Identifier
boundTypeIdentifier =
  boundTypeIdentifier' . S.strip
  where
    boundTypeIdentifier' (S.DataDefinition i _)    = S.singleton i
    boundTypeIdentifier' (S.TypeBind i _)          = S.singleton i
    boundTypeIdentifier' (S.ForeignTypeBind i _ _) = S.singleton i
    boundTypeIdentifier' _                         = S.empty

importedAffiliations :: Map S.ModuleName (Set S.Identifier) -> Set S.ModuleName -> Map S.Identifier Affiliation
importedAffiliations identifiers importedModuleNames =
  M.unions $ S.map (\n -> M.fromSet (const $ Global n) $ identifiers M.! n) importedModuleNames

data Affiliation
  = Global S.ModuleName
  | Local
  deriving (Show, Read, Eq, Ord, Generic)

data Exception
  = UnknownIdentifierException S.EitherIdentifier (Maybe (S.Position, S.Position))
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
