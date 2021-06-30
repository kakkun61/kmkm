{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Language.Kmkm.Build.NameResolve
  ( nameResolve
  , boundIdentifiers
  , Exception (..)
  ) where

import qualified Language.Kmkm.Syntax as S

import qualified Control.Exception       as E
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (mapMaybe)
import           Data.Set                (Set)
import qualified Data.Set                as S
import qualified Data.Typeable           as Y
import           GHC.Generics            (Generic)
import qualified Language.Kmkm.Exception as X

type Module n = S.Module n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Definition n = S.Definition n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type ValueBind n = S.ValueBind n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Value n = S.Value n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Value' n = S.Value' n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type ProcedureStep n = S.ProcedureStep n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Literal n = S.Literal n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type TypeAnnotation n = S.TypeAnnotation n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Application n = S.Application n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Function n = S.Function n 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Type n = S.Type n 'S.Curried

nameResolve :: MonadThrow m => Map S.ModuleName (Set S.Identifier) -> Map S.ModuleName (Set S.Identifier) -> Module 'S.NameUnresolved -> m (Module 'S.NameResolved)
nameResolve valueIdentifiers typeIdentifiers (S.Module moduleName ms ds) = do
  let
    valueIdentifiers' = M.insert moduleName (S.fromList $ mapMaybe boundValueIdentifier ds) valueIdentifiers
    valueAffiliations = importedAffiliations valueIdentifiers' $ S.fromList ms
    typeIdentifiers' = M.insert moduleName (S.fromList $ mapMaybe boundTypeIdentifier ds) typeIdentifiers
    typeAffiliations = importedAffiliations typeIdentifiers' $ S.fromList ms
  S.Module moduleName ms <$> sequence (definition valueAffiliations typeAffiliations moduleName <$> ds)

definition :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Definition 'S.NameUnresolved -> m (Definition 'S.NameResolved)
definition valueAffiliations typeAffiliations moduleName (S.ValueBind b) = S.ValueBind <$> valueBind valueAffiliations typeAffiliations moduleName b
definition _ typeAffiliations moduleName (S.TypeBind i t) = S.TypeBind (S.GlobalIdentifier moduleName i) <$> typ typeAffiliations moduleName t
definition _ _ moduleName (S.ForeignTypeBind i hs c) = pure $ S.ForeignTypeBind (S.GlobalIdentifier moduleName i) hs c
definition _ typeAffiliations moduleName (S.DataDefinition i cs) =
  S.DataDefinition (S.GlobalIdentifier moduleName i) <$> sequence (constructor <$> cs)
  where
    constructor (i, fs) = (S.GlobalIdentifier moduleName i,) <$> sequence (field <$> fs)
    field (i, t) = (S.GlobalIdentifier moduleName i,) <$> typ typeAffiliations moduleName t
definition _ typeAffiliations moduleName (S.ForeignValueBind i hs c t) = S.ForeignValueBind (S.GlobalIdentifier moduleName i) hs c <$> typ typeAffiliations moduleName t

valueBind :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> ValueBind 'S.NameUnresolved -> m (ValueBind 'S.NameResolved)
valueBind valueAffiliations typeAffiliations moduleName (S.ValueBindU i v) = S.ValueBindU (S.GlobalIdentifier moduleName i) <$> value valueAffiliations typeAffiliations moduleName v

value :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Value 'S.NameUnresolved -> m (Value 'S.NameResolved)
value valueAffiliations typeAffiliations moduleName (S.UntypedValue v) = S.UntypedValue <$> value' valueAffiliations typeAffiliations moduleName v

value' :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Value' 'S.NameUnresolved -> m (Value' 'S.NameResolved)
value' valueAffiliations _ moduleName (S.Variable i) = S.Variable <$> referenceIdentifier valueAffiliations moduleName i
value' valueAffiliations typeAffiliations moduleName (S.Procedure ss) = S.Procedure <$> sequence (procedureStep valueAffiliations typeAffiliations moduleName <$> ss)
value' valueAffiliations typeAffiliations moduleName (S.Literal l) = S.Literal <$> literal valueAffiliations typeAffiliations moduleName l
value' valueAffiliations typeAffiliations moduleName (S.TypeAnnotation a) = S.TypeAnnotation <$> typeAnnotation valueAffiliations typeAffiliations moduleName a
value' valueAffiliations typeAffiliations moduleName (S.Application a) = S.Application <$> application valueAffiliations typeAffiliations moduleName a
value' valueAffiliations typeAffiliations moduleName (S.Let ds v) = S.Let <$> sequence (definition valueAffiliations typeAffiliations moduleName <$> ds) <*> value valueAffiliations typeAffiliations moduleName v

procedureStep :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> ProcedureStep 'S.NameUnresolved -> m (ProcedureStep 'S.NameResolved)
procedureStep valueAffiliations typeAffiliations moduleName (S.TermProcedure v) = S.TermProcedure <$> value valueAffiliations typeAffiliations moduleName v
procedureStep valueAffiliations typeAffiliations moduleName (S.BindProcedure i v) = S.BindProcedure (S.GlobalIdentifier moduleName i) <$> value valueAffiliations typeAffiliations moduleName v

literal :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Literal 'S.NameUnresolved -> m (Literal 'S.NameResolved)
literal _ _ _ (S.Integer v b)                    = pure $ S.Integer v b
literal _ _ _ (S.Fraction f s e b)               = pure $ S.Fraction f s e b
literal _ _ _ (S.String s)                       = pure $ S.String s
literal valueAffiliations typeAffiliations moduleName (S.Function f) = S.Function <$> function valueAffiliations typeAffiliations moduleName f

typeAnnotation :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> TypeAnnotation 'S.NameUnresolved -> m (TypeAnnotation 'S.NameResolved)
typeAnnotation valueAffiliations typeAffiliations moduleName (S.TypeAnnotation' v t) = S.TypeAnnotation' <$> value valueAffiliations typeAffiliations moduleName v <*> typ valueAffiliations moduleName t

application :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Application 'S.NameUnresolved -> m (Application 'S.NameResolved)
application valueAffiliations typeAffiliations moduleName (S.ApplicationC v1 v2) = S.ApplicationC <$> value valueAffiliations typeAffiliations moduleName v1 <*> value valueAffiliations typeAffiliations moduleName v2

function :: MonadThrow m => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Function 'S.NameUnresolved -> m (Function 'S.NameResolved)
function valueAffiliations typeAffiliations moduleName (S.FunctionC i t v) = S.FunctionC (S.LocalIdentifier i) <$> typ valueAffiliations moduleName t <*> value valueAffiliations typeAffiliations moduleName v

typ :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Type 'S.NameUnresolved -> m (Type 'S.NameResolved)
typ typeAffiliations moduleName (S.TypeVariable i)  = S.TypeVariable <$> referenceIdentifier typeAffiliations moduleName i
typ typeAffiliations moduleName (S.ProcedureType t) = S.ProcedureType <$> typ typeAffiliations moduleName t
typ _ _ v                                       = error $ show v

referenceIdentifier :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> S.ReferenceIdentifier 'S.NameUnresolved -> m (S.ReferenceIdentifier 'S.NameResolved)
referenceIdentifier valueAffiliations _ i@(S.UnqualifiedIdentifier i') =
  case M.lookup i' valueAffiliations of
    Just (Global n) -> pure $ S.GlobalIdentifier n i'
    Just Local      -> pure $ S.LocalIdentifier i'
    Nothing         -> throwM $ UnknownIdentifierException i
referenceIdentifier valueAffiliations _ i@(S.QualifiedIdentifier i'@(S.GlobalIdentifier n i'')) =
  case M.lookup i'' valueAffiliations of
    Just (Global n') | n == n' -> pure i'
    _ -> throwM $ UnknownIdentifierException i
referenceIdentifier _ _ (S.QualifiedIdentifier i) = pure i

boundIdentifiers :: Functor f => f (Module 'S.NameUnresolved) -> (f (Set S.Identifier), f (Set S.Identifier))
boundIdentifiers modules =
  (eachModule boundValueIdentifier <$> modules, eachModule boundTypeIdentifier <$> modules)
  where
    eachModule f (S.Module _ _ ds) = S.fromList $ mapMaybe f ds

boundValueIdentifier :: Definition 'S.NameUnresolved -> Maybe S.Identifier
boundValueIdentifier (S.ValueBind (S.ValueBindU i _)) = Just i
boundValueIdentifier (S.ForeignValueBind i _ _ _) = Just i
boundValueIdentifier _                                = Nothing

boundTypeIdentifier :: Definition 'S.NameUnresolved -> Maybe S.Identifier
boundTypeIdentifier (S.TypeBind i _) = Just i
boundTypeIdentifier (S.ForeignTypeBind i _ _) = Just i
boundTypeIdentifier _                                = Nothing

importedAffiliations :: Map S.ModuleName (Set S.Identifier) -> Set S.ModuleName -> Map S.Identifier Affiliation
importedAffiliations identifiers importedModuleNames =
  M.unions $ S.map (\n -> M.fromSet (const $ Global n) $ identifiers M.! n) importedModuleNames

data Affiliation
  = Global S.ModuleName
  | Local
  deriving (Show, Read, Eq, Ord, Generic)

newtype Exception
  = UnknownIdentifierException S.EitherIdentifier
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
