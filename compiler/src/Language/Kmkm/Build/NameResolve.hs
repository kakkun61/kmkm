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

nameResolve :: MonadThrow m => Map S.ModuleName (Set S.Identifier) -> Module 'S.NameUnresolved -> m (Module 'S.NameResolved)
nameResolve identifiers (S.Module moduleName ms ds) = do
  let
    identifiers' = M.insert moduleName (S.fromList $ mapMaybe boundIdentifier ds) identifiers
    affiliations = importedAffiliations identifiers' $ S.fromList ms
  S.Module moduleName ms <$> sequence (definition affiliations moduleName <$> ds)

definition :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Definition 'S.NameUnresolved -> m (Definition 'S.NameResolved)
definition affiliations moduleName (S.ValueBind b) = S.ValueBind <$> valueBind affiliations moduleName b
definition affiliations moduleName (S.TypeBind i t) = S.TypeBind (S.GlobalIdentifier moduleName i) <$> typ affiliations moduleName t
definition _ moduleName (S.ForeignTypeBind i hs c) = pure $ S.ForeignTypeBind (S.GlobalIdentifier moduleName i) hs c
definition affiliations moduleName (S.DataDefinition i cs) =
  S.DataDefinition (S.GlobalIdentifier moduleName i) <$> sequence (constructor <$> cs)
  where
    constructor (i, fs) = (S.GlobalIdentifier moduleName i,) <$> sequence (field <$> fs)
    field (i, t) = (S.GlobalIdentifier moduleName i,) <$> typ affiliations moduleName t
definition affiliations moduleName (S.ForeignValueBind i hs c t) = S.ForeignValueBind (S.GlobalIdentifier moduleName i) hs c <$> typ affiliations moduleName t

valueBind :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> ValueBind 'S.NameUnresolved -> m (ValueBind 'S.NameResolved)
valueBind affiliations moduleName (S.ValueBindU i v) = S.ValueBindU (S.GlobalIdentifier moduleName i) <$> value affiliations moduleName v

value :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Value 'S.NameUnresolved -> m (Value 'S.NameResolved)
value affiliations moduleName (S.UntypedValue v) = S.UntypedValue <$> value' affiliations moduleName v

value' :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Value' 'S.NameUnresolved -> m (Value' 'S.NameResolved)
value' affiliations moduleName (S.Variable i) = S.Variable <$> referenceIdentifier affiliations moduleName i
value' affiliations moduleName (S.Procedure ss) = S.Procedure <$> sequence (procedureStep affiliations moduleName <$> ss)
value' affiliations moduleName (S.Literal l) = S.Literal <$> literal affiliations moduleName l
value' affiliations moduleName (S.TypeAnnotation a) = S.TypeAnnotation <$> typeAnnotation affiliations moduleName a
value' affiliations moduleName (S.Application a) = S.Application <$> application affiliations moduleName a
value' affiliations moduleName (S.Let ds v) = S.Let <$> sequence (definition affiliations moduleName <$> ds) <*> value affiliations moduleName v

procedureStep :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> ProcedureStep 'S.NameUnresolved -> m (ProcedureStep 'S.NameResolved)
procedureStep affiliations moduleName (S.TermProcedure v) = S.TermProcedure <$> value affiliations moduleName v
procedureStep affiliations moduleName (S.BindProcedure i v) = S.BindProcedure (S.GlobalIdentifier moduleName i) <$> value affiliations moduleName v

literal :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Literal 'S.NameUnresolved -> m (Literal 'S.NameResolved)
literal _ _ (S.Integer v b)                    = pure $ S.Integer v b
literal _ _ (S.Fraction f s e b)               = pure $ S.Fraction f s e b
literal _ _ (S.String s)                       = pure $ S.String s
literal affiliations moduleName (S.Function f) = S.Function <$> function affiliations moduleName f

typeAnnotation :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> TypeAnnotation 'S.NameUnresolved -> m (TypeAnnotation 'S.NameResolved)
typeAnnotation affiliations moduleName (S.TypeAnnotation' v t) = S.TypeAnnotation' <$> value affiliations moduleName v <*> typ affiliations moduleName t

application :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Application 'S.NameUnresolved -> m (Application 'S.NameResolved)
application affiliations moduleName (S.ApplicationC v1 v2) = S.ApplicationC <$> value affiliations moduleName v1 <*> value affiliations moduleName v2

function :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Function 'S.NameUnresolved -> m (Function 'S.NameResolved)
function affiliations moduleName (S.FunctionC i t v) = S.FunctionC (S.LocalIdentifier i) <$> typ affiliations moduleName t <*> value affiliations moduleName v

typ :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> Type 'S.NameUnresolved -> m (Type 'S.NameResolved)
typ affiliations moduleName (S.TypeVariable i)  = S.TypeVariable <$> referenceIdentifier affiliations moduleName i
typ affiliations moduleName (S.ProcedureType t) = S.ProcedureType <$> typ affiliations moduleName t
typ _ _ v                                       = error $ show v

referenceIdentifier :: MonadThrow m => Map S.Identifier Affiliation -> S.ModuleName -> S.ReferenceIdentifier 'S.NameUnresolved -> m (S.ReferenceIdentifier 'S.NameResolved)
referenceIdentifier affiliations _ i@(S.UnqualifiedIdentifier i') =
  case M.lookup i' affiliations of
    Just (Global n) -> pure $ S.GlobalIdentifier n i'
    Just Local      -> pure $ S.LocalIdentifier i'
    Nothing         -> throwM $ UnknownException i
referenceIdentifier _ _ (S.QualifiedIdentifier i) = pure i

boundIdentifiers :: Functor f => f (Module 'S.NameUnresolved) -> f (Set S.Identifier)
boundIdentifiers modules =
  eachModule <$> modules
  where
    eachModule (S.Module _ _ ds) = S.fromList $ mapMaybe boundIdentifier ds

boundIdentifier :: Definition 'S.NameUnresolved -> Maybe S.Identifier
boundIdentifier (S.ValueBind (S.ValueBindU i _)) = Just i
boundIdentifier _                                = Nothing

importedAffiliations :: Map S.ModuleName (Set S.Identifier) -> Set S.ModuleName -> Map S.Identifier Affiliation
importedAffiliations identifiers importedModuleNames =
  M.unions $ S.map (\n -> M.fromSet (const $ Global n) $ identifiers M.! n) importedModuleNames

data Affiliation
  = Global S.ModuleName
  | Local
  deriving (Show, Read, Eq, Ord, Generic)

newtype Exception
  = UnknownException S.EitherIdentifier
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
