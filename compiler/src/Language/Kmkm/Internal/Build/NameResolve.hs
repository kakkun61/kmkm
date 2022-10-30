{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.Kmkm.Internal.Build.NameResolve
  ( nameResolve
  , boundIdentifiers
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Syntax.Core.Common                                        as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Untyped.Curried.LambdaUnlifted   as S2
import qualified Language.Kmkm.Internal.Syntax.Core.NameUnresolved.Untyped.Curried.LambdaUnlifted as S1

import qualified Control.Exception                as E
import           Control.Exception.Safe           (MonadThrow, throw)
import           Data.Copointed                   (Copointed (copoint))
import qualified Data.Functor.Barbie.Layered      as B
import           Data.Functor.F                   (F (F), unf)
import           Data.Functor.Identity            (Identity (Identity, runIdentity))
import           Data.Functor.With                (MayHave)
import qualified Data.Functor.With                as W
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import qualified Data.Typeable                    as Y
import           GHC.Generics                     (Generic)
import qualified Language.Kmkm.Internal.Exception as X

nameResolve
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map SC.ModuleName (Set SC.Identifier)
  -> Map SC.ModuleName (Set SC.Identifier)
  -> F f (S1.Module et ev f)
  -> m (F f (S2.Module et ev f))
nameResolve valueIdentifiers typeIdentifiers =
  traverse $ \(S1.Module moduleName ms ds) -> do
    let
      moduleName' = copoint moduleName
      ms' = moduleName' : (copoint <$> copoint ms)
      valueIdentifiers' = M.insert moduleName' (S.unions $ boundValueIdentifier <$> copoint ds) valueIdentifiers
      valueAffiliations = importedAffiliations valueIdentifiers' $ S.fromList ms'
      typeIdentifiers' = M.insert moduleName' (S.unions $ boundTypeIdentifier <$> copoint ds) typeIdentifiers
      typeAffiliations = importedAffiliations typeIdentifiers' $ S.fromList ms'
    S2.Module moduleName ms <$> mapM (traverse $ definition valueAffiliations typeAffiliations moduleName' $ Global moduleName') ds

definition
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map SC.Identifier Affiliation
  -> Map SC.Identifier Affiliation
  -> SC.ModuleName
  -> Affiliation
  -> F f (S1.Definition et ev f)
  -> m (F f (S2.Definition et ev f))
definition valueAffiliations typeAffiliations moduleName affiliation =
  traverse $ \case
    S1.ValueBind i v ->
      case affiliation of
        Global _ -> S2.ValueBind (SC.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v
        Local -> S2.ValueBind (SC.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations moduleName v
    S1.TypeBind i t -> S2.TypeBind (SC.GlobalIdentifier moduleName <$> i) <$> typ typeAffiliations moduleName t
    S1.ForeignTypeBind i c -> pure $ S2.ForeignTypeBind (SC.GlobalIdentifier moduleName <$> i) c
    S1.DataDefinition i r -> S2.DataDefinition (SC.GlobalIdentifier moduleName <$> i) <$> dataRepresentation typeAffiliations moduleName r
    S1.ForeignValueBind i c t -> S2.ForeignValueBind (SC.GlobalIdentifier moduleName <$> i) c <$> typ typeAffiliations moduleName t

dataRepresentation :: (MonadThrow m, Traversable f, Copointed f, MayHave SC.Location f) => Map SC.Identifier Affiliation -> SC.ModuleName -> F f (S1.DataRepresentation et ev f) -> m (F f (S2.DataRepresentation et ev f))
dataRepresentation typeAffiliations moduleName =
  traverse $ \case
    S1.ForAllData i r -> S2.ForAllData (SC.LocalIdentifier <$> i) <$> dataRepresentation (M.insert (copoint i) Local typeAffiliations) moduleName r
    S1.ValueConstructorsData cs -> S2.ValueConstructorsData <$> mapM (traverse $ valueConstructor typeAffiliations moduleName) cs

valueConstructor :: (MonadThrow m, Traversable f, Copointed f, MayHave SC.Location f) => Map SC.Identifier Affiliation -> SC.ModuleName -> F f (S1.ValueConstructor et ev f) -> m (F f (S2.ValueConstructor et ev f))
valueConstructor typeAffiliations moduleName =
  traverse $ \(S1.ValueConstructor i fs) -> do
    fs' <- mapM (traverse $ field typeAffiliations moduleName) fs
    pure $ S2.ValueConstructor (SC.GlobalIdentifier moduleName <$> i) fs'

field :: (MonadThrow m, Traversable f, Copointed f, MayHave SC.Location f) => Map SC.Identifier Affiliation -> SC.ModuleName -> F f (S1.Field et ev f) -> m (F f (S2.Field et ev f))
field typeAffiliations moduleName =
  traverse $ \(S1.Field i t) -> do
    t' <- typ typeAffiliations moduleName t
    pure $ S2.Field (SC.GlobalIdentifier moduleName <$> i) t'

value
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map SC.Identifier Affiliation
  -> Map SC.Identifier Affiliation
  -> SC.ModuleName
  -> F f (S1.Value et ev f)
  -> m (F f (S2.Value et ev f))
value valueAffiliations typeAffiliations moduleName =
  traverse $ \case
    S1.Variable i -> S2.Variable <$> referenceIdentifier valueAffiliations i
    S1.Procedure ss -> S2.Procedure <$> mapM (traverse $ procedureStep valueAffiliations typeAffiliations moduleName) ss
    S1.Literal l -> pure $ S2.Literal $ literal <$> l
    S1.Function i t v ->
      let valueAffiliations' = M.insert (copoint i) Local valueAffiliations
      in S2.Function (SC.LocalIdentifier <$> i) <$> typ typeAffiliations moduleName t <*> value valueAffiliations' typeAffiliations moduleName v
    S1.TypeAnnotation v t -> S2.TypeAnnotation <$> value valueAffiliations typeAffiliations moduleName v <*> typ typeAffiliations moduleName t
    S1.Application v1 v2 -> S2.Application <$> value valueAffiliations typeAffiliations moduleName v1 <*> value valueAffiliations typeAffiliations moduleName v2
    S1.Let ds v ->
      let
        localIdentifiers = S.unions $ boundValueIdentifier <$> copoint ds
        localAffiliations = M.fromSet (const Local) localIdentifiers
        valueAffiliations' = M.union localAffiliations valueAffiliations
      in
        S2.Let
          <$> mapM (traverse $ definition valueAffiliations' typeAffiliations moduleName Local) ds
          <*> value valueAffiliations' typeAffiliations moduleName v
    S1.ForAllValue i v ->
      let typeAffiliations' = M.insert (copoint i) Local typeAffiliations
      in S2.ForAllValue (SC.LocalIdentifier <$> i) <$> value valueAffiliations typeAffiliations' moduleName v
    S1.Instantiation v t -> S2.Instantiation <$> value valueAffiliations typeAffiliations moduleName v <*> typ typeAffiliations moduleName t

procedureStep
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map SC.Identifier Affiliation
  -> Map SC.Identifier Affiliation
  -> SC.ModuleName
  -> F f (S1.ProcedureStep et ev f)
  -> m (F f (S2.ProcedureStep et ev f))
procedureStep valueAffiliations typeAffiliations moduleName =
  traverse $ \case
    S1.CallProcedureStep v   -> S2.CallProcedureStep <$> value valueAffiliations typeAffiliations moduleName v
    S1.BindProcedureStep i v -> S2.BindProcedureStep (SC.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v

literal :: S1.Literal -> S2.Literal
literal (S1.Integer v b)      = S2.Integer v b
literal (S1.Fraction f s e b) = S2.Fraction f s e b
literal (S1.String s)         = S2.String s

typ
  :: ( MonadThrow m
     , Traversable f
     , Copointed f
     , MayHave SC.Location f
     )
  => Map SC.Identifier Affiliation
  -> SC.ModuleName
  -> F f (S1.Type f)
  -> m (F f (S2.Type f))
typ typeAffiliations moduleName =
  traverse $ \case
    S1.TypeVariable i  -> S2.TypeVariable <$> referenceIdentifier typeAffiliations i
    S1.ProcedureType t -> S2.ProcedureType <$> typ typeAffiliations moduleName t
    S1.TypeApplication t1 t2 -> S2.TypeApplication <$> typ typeAffiliations moduleName t1 <*> typ typeAffiliations moduleName t2
    S1.FunctionType t1 t2 -> S2.FunctionType <$> typ typeAffiliations moduleName t1 <*> typ typeAffiliations moduleName t2
    S1.ForAllType i t -> do
      let typeAffiliations' = M.insert (copoint i) Local typeAffiliations
      S2.ForAllType (SC.LocalIdentifier <$> i) <$> typ typeAffiliations' moduleName t

referenceIdentifier
  :: ( MonadThrow m
     , Functor f
     , Copointed f
     , MayHave SC.Location f
     )
  => Map SC.Identifier Affiliation
  -> F f S1.ReferenceIdentifier
  -> m (F f S2.ReferenceIdentifier)
referenceIdentifier affiliations i =
  case copoint i of
    SC.UnqualifiedIdentifier i' ->
      case M.lookup i' affiliations of
        Just (Global n) -> pure $ SC.GlobalIdentifier n i' <$ i
        Just Local      -> pure $ SC.LocalIdentifier i' <$ i
        Nothing         -> throw $ UnknownIdentifierException (copoint i) $ W.mayGet i
    SC.QualifiedIdentifier i'@(SC.GlobalIdentifier n i'') ->
      case M.lookup i'' affiliations of
        Just (Global n') | n == n' -> pure $ i' <$ i
        _                          -> throw $ UnknownIdentifierException (copoint i) $ W.mayGet i
    SC.QualifiedIdentifier i' -> pure $ i' <$ i

boundIdentifiers :: (Functor f, Functor g, Copointed g, B.FunctorB et, B.FunctorB ev) => f (F g (S1.Module et ev g)) -> (f (Set SC.Identifier), f (Set SC.Identifier))
boundIdentifiers modules =
  (eachModule boundValueIdentifier <$> modules, eachModule boundTypeIdentifier <$> modules)
  where
    eachModule f m = let S1.Module _ _ ds = copoint m in S.unions $ f <$> copoint ds

boundValueIdentifier :: (Functor f, Copointed f, B.FunctorB et, B.FunctorB ev) => F f (S1.Definition et ev f) -> Set SC.Identifier
boundValueIdentifier =
  boundValueIdentifier' . runIdentity . SC.toIdentity . unf
  where
    boundValueIdentifier' (S1.DataDefinition _ (F (Identity r)))     = identifiers r
    boundValueIdentifier' (S1.ValueBind (F (Identity i)) _)          = S.singleton i
    boundValueIdentifier' (S1.ForeignValueBind (F (Identity i)) _ _) = S.singleton i
    boundValueIdentifier' _                                          = S.empty
    identifiers (S1.ForAllData _ (F (Identity r))) = identifiers r
    identifiers (S1.ValueConstructorsData (F (Identity cs)))               = S.fromList $ (\(F (Identity (S1.ValueConstructor (F (Identity i)) _))) -> i) <$> cs

boundTypeIdentifier :: (Functor f, Copointed f, B.FunctorB et, B.FunctorB ev) => F f (S1.Definition et ev f) -> Set SC.Identifier
boundTypeIdentifier =
  boundTypeIdentifier' . runIdentity . SC.toIdentity . unf
  where
    boundTypeIdentifier' (S1.DataDefinition (F (Identity i)) _)  = S.singleton i
    boundTypeIdentifier' (S1.TypeBind (F (Identity i)) _)        = S.singleton i
    boundTypeIdentifier' (S1.ForeignTypeBind (F (Identity i)) _) = S.singleton i
    boundTypeIdentifier' _                                       = S.empty

importedAffiliations :: Map SC.ModuleName (Set SC.Identifier) -> Set SC.ModuleName -> Map SC.Identifier Affiliation
importedAffiliations identifiers importedModuleNames =
  M.unions $ S.map (\n -> M.fromSet (const $ Global n) $ identifiers M.! n) importedModuleNames

data Affiliation
  = Global SC.ModuleName
  | Local
  deriving (Show, Read, Eq, Ord, Generic)

data Exception
  = UnknownIdentifierException SC.EitherIdentifier (Maybe SC.Location)
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
