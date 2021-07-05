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

type Module n f = S.Module n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Definition n f = S.Definition n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type ValueBind n f = S.ValueBind n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Value n f = S.Value n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Value' n f = S.Value' n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type ProcedureStep n f = S.ProcedureStep n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Literal n f = S.Literal n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type TypeAnnotation n f = S.TypeAnnotation n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Application n f = S.Application n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Function n f = S.Function n 'S.Curried 'S.LambdaUnlifted 'S.Untyped f

type Type n f = S.Type n 'S.Curried f

nameResolve
  :: ( MonadThrow m
     , S.HasPosition f
     )
  => Map S.ModuleName (Set S.Identifier)
  -> Map S.ModuleName (Set S.Identifier)
  -> f (S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped f)
  -> m (f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped f))
nameResolve valueIdentifiers typeIdentifiers = traverse (module' valueIdentifiers typeIdentifiers)

module' :: (MonadThrow m, S.HasPosition f) => Map S.ModuleName (Set S.Identifier) -> Map S.ModuleName (Set S.Identifier) -> Module 'S.NameUnresolved f -> m (Module 'S.NameResolved f)
module' valueIdentifiers typeIdentifiers (S.Module moduleName ms ds) = do
  let
    moduleName' = S.item moduleName
    ms' = S.item <$> ms
    valueIdentifiers' = M.insert moduleName' (S.fromList $ mapMaybe boundValueIdentifier ds) valueIdentifiers
    valueAffiliations = importedAffiliations valueIdentifiers' $ S.fromList ms'
    typeIdentifiers' = M.insert moduleName' (S.fromList $ mapMaybe boundTypeIdentifier ds) typeIdentifiers
    typeAffiliations = importedAffiliations typeIdentifiers' $ S.fromList ms'
  S.Module moduleName ms <$> sequence (definition valueAffiliations typeAffiliations moduleName' <$> ds)

definition :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> f (Definition 'S.NameUnresolved f) -> m (f (Definition 'S.NameResolved f))
definition valueAffiliations typeAffiliations moduleName =
  traverse go
  where
    go (S.ValueBind b) = S.ValueBind <$> valueBind valueAffiliations typeAffiliations moduleName b
    go (S.TypeBind i t) = S.TypeBind (S.GlobalIdentifier moduleName <$> i) <$> traverse (typ typeAffiliations moduleName) t
    go (S.ForeignTypeBind i hs c) = pure $ S.ForeignTypeBind (S.GlobalIdentifier moduleName <$> i) hs c
    go (S.DataDefinition i cs) =
      S.DataDefinition (S.GlobalIdentifier moduleName <$> i) <$> sequence (constructor <$> cs)
      where
        constructor (i, fs) = (S.GlobalIdentifier moduleName <$> i,) <$> sequence (field <$> fs)
        field (i, t) = (S.GlobalIdentifier moduleName <$> i,) <$> traverse (typ typeAffiliations moduleName) t
    go (S.ForeignValueBind i hs c t) = S.ForeignValueBind (S.GlobalIdentifier moduleName <$> i) hs c <$> traverse (typ typeAffiliations moduleName) t

valueBind :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> ValueBind 'S.NameUnresolved f -> m (ValueBind 'S.NameResolved f)
valueBind valueAffiliations typeAffiliations moduleName (S.ValueBindU i v) = S.ValueBindU (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v

value :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> f (Value 'S.NameUnresolved f) -> m (f (Value 'S.NameResolved f))
value valueAffiliations typeAffiliations moduleName v =
  traverse go v
  where
    go (S.UntypedValue v) = S.UntypedValue <$> value' valueAffiliations typeAffiliations moduleName v

value' :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> f (Value' 'S.NameUnresolved f) -> m (f (Value' 'S.NameResolved f))
value' valueAffiliations typeAffiliations moduleName v =
  traverse go v
  where
    go (S.Variable i) = S.Variable . S.item <$> referenceIdentifier valueAffiliations moduleName (i <$ v)
    go (S.Procedure ss) = S.Procedure <$> sequence (traverse (procedureStep valueAffiliations typeAffiliations moduleName) <$> ss)
    go (S.Literal l) = S.Literal <$> literal valueAffiliations typeAffiliations moduleName l
    go (S.TypeAnnotation a) = S.TypeAnnotation <$> typeAnnotation valueAffiliations typeAffiliations moduleName a
    go (S.Application a) = S.Application <$> application valueAffiliations typeAffiliations moduleName a
    go (S.Let ds v) = S.Let <$> sequence (definition valueAffiliations typeAffiliations moduleName <$> ds) <*> value valueAffiliations typeAffiliations moduleName v

procedureStep :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> ProcedureStep 'S.NameUnresolved f -> m (ProcedureStep 'S.NameResolved f)
procedureStep valueAffiliations typeAffiliations moduleName (S.TermProcedure v) = S.TermProcedure <$> value valueAffiliations typeAffiliations moduleName v
procedureStep valueAffiliations typeAffiliations moduleName (S.BindProcedure i v) = S.BindProcedure (S.GlobalIdentifier moduleName <$> i) <$> value valueAffiliations typeAffiliations moduleName v

literal :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Literal 'S.NameUnresolved f -> m (Literal 'S.NameResolved f)
literal _ _ _ (S.Integer v b)                    = pure $ S.Integer v b
literal _ _ _ (S.Fraction f s e b)               = pure $ S.Fraction f s e b
literal _ _ _ (S.String s)                       = pure $ S.String s
literal valueAffiliations typeAffiliations moduleName (S.Function f) = S.Function <$> function valueAffiliations typeAffiliations moduleName f

typeAnnotation :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> TypeAnnotation 'S.NameUnresolved f -> m (TypeAnnotation 'S.NameResolved f)
typeAnnotation valueAffiliations typeAffiliations moduleName (S.TypeAnnotation' v t) = S.TypeAnnotation' <$> value valueAffiliations typeAffiliations moduleName v <*> traverse (typ valueAffiliations moduleName) t

application :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Application 'S.NameUnresolved f -> m (Application 'S.NameResolved f)
application valueAffiliations typeAffiliations moduleName (S.ApplicationC v1 v2) = S.ApplicationC <$> value valueAffiliations typeAffiliations moduleName v1 <*> value valueAffiliations typeAffiliations moduleName v2

function :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> Map S.Identifier Affiliation -> S.ModuleName -> Function 'S.NameUnresolved f -> m (Function 'S.NameResolved f)
function valueAffiliations typeAffiliations moduleName (S.FunctionC i t v) = S.FunctionC (S.LocalIdentifier <$> i) <$> traverse (typ valueAffiliations moduleName) t <*> value valueAffiliations typeAffiliations moduleName v

typ :: (MonadThrow m, S.HasPosition f) => Map S.Identifier Affiliation -> S.ModuleName -> Type 'S.NameUnresolved f -> m (Type 'S.NameResolved f)
typ typeAffiliations moduleName (S.TypeVariable i)  = S.TypeVariable <$>  referenceIdentifier typeAffiliations moduleName i
typ typeAffiliations moduleName (S.ProcedureType t) = S.ProcedureType <$> traverse (typ typeAffiliations moduleName) t
typ _ _ _ = undefined

referenceIdentifier :: MonadThrow m => S.HasPosition f => Map S.Identifier Affiliation -> S.ModuleName -> f (S.ReferenceIdentifier 'S.NameUnresolved) -> m (f (S.ReferenceIdentifier 'S.NameResolved))
referenceIdentifier valueAffiliations _ i =
  case S.item i of
    S.UnqualifiedIdentifier i' ->
      case M.lookup i' valueAffiliations of
        Just (Global n) -> pure $ S.GlobalIdentifier n i' <$ i
        Just Local      -> pure $ S.LocalIdentifier i' <$ i
        Nothing         -> throwM $ UnknownIdentifierException (S.item i) $ S.range i
    S.QualifiedIdentifier i'@(S.GlobalIdentifier n i'') ->
      case M.lookup i'' valueAffiliations of
        Just (Global n') | n == n' -> pure $ i' <$ i
        _                          -> throwM $ UnknownIdentifierException (S.item i) $ S.range i
    S.QualifiedIdentifier i' -> pure $ i' <$ i

boundIdentifiers :: (Functor f, S.HasPosition g) => f (g (Module 'S.NameUnresolved g)) -> (f (Set S.Identifier), f (Set S.Identifier))
boundIdentifiers modules =
  (eachModule boundValueIdentifier <$> modules, eachModule boundTypeIdentifier <$> modules)
  where
    eachModule f m = let S.Module _ _ ds = S.item m in S.fromList $ mapMaybe f ds

boundValueIdentifier :: S.HasPosition f => f (Definition 'S.NameUnresolved f) -> Maybe S.Identifier
boundValueIdentifier d =
  boundValueIdentifier' $ S.item d
  where
    boundValueIdentifier' (S.ValueBind (S.ValueBindU i _)) = Just $ S.item i
    boundValueIdentifier' (S.ForeignValueBind i _ _ _)     = Just $ S.item i
    boundValueIdentifier' _                                = Nothing

boundTypeIdentifier :: S.HasPosition f => f (Definition 'S.NameUnresolved f) -> Maybe S.Identifier
boundTypeIdentifier d =
  boundTypeIdentifier' $ S.item d
  where
    boundTypeIdentifier' (S.TypeBind i _)          = Just $ S.item i
    boundTypeIdentifier' (S.ForeignTypeBind i _ _) = Just $ S.item i
    boundTypeIdentifier' _                         = Nothing

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
