{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

-- | “top-level thunk / compile-time expression” pass.
module Language.Kmkm.Internal.Build.C.Thunk
  ( thunk
  ) where

import qualified Language.Kmkm.Internal.Syntax as S

import           Data.Copointed     (Copointed (copoint))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S

-- XXX: smarter method wanted
-- It is not necessary to convert all top-level value definitions,
-- but to do non-compile-time expressions only.
-- I think that a smarter method needs topological sort for dependencies
-- of value definitions.

type Definition = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

type ValueBind = S.ValueBind 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

type Type = S.Type 'S.NameResolved 'S.Uncurried

type Value = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

type ProcedureStep = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

thunk
  :: ( Functor f
     , Copointed f
     , S.HasLocation f
     )
  => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue f)
  -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue f)
thunk = fmap $ \(S.Module n ms ds) -> S.Module n ms $ fmap (definition (thunkIdentifiers ds)) <$> ds

definition :: (Functor f, Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f (Definition f) -> f (Definition f)
definition tids d =
  case copoint d of
    S.ValueBind b | S.ValueBindV i v <- copoint b -> definition' tids (S.ValueBind (S.ValueBindN i ([] <$ d) v <$ b) <$ d)
    _                              -> definition' tids d

definition' :: (Functor f, Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f (Definition f) -> f (Definition f)
definition' tids =
  fmap $ \case
    S.ValueBind b ->
      S.ValueBind $
        flip fmap b $ \case
          S.ValueBindV i v    -> valueBind tids i Nothing v
          S.ValueBindN i ps v -> valueBind tids i (Just ps) v
    m                                 -> m

valueBind :: (Functor f, Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f S.QualifiedIdentifier -> Maybe (f [f (f S.QualifiedIdentifier, f (Type f))]) -> f (Value f) -> ValueBind f
valueBind tids i mps v =
  let v' = term tids v
  in maybe (S.ValueBindV i v') (\ps -> S.ValueBindN i ps v') mps

term :: (Functor f, Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f (Value f) -> f (Value f)
term tids v =
  let S.TypedValue v1 t = copoint v
  in
    case copoint v1 of
      S.Variable i
        | copoint i `S.member` tids -> S.TypedValue (S.Application (S.ApplicationN (S.TypedValue (S.Variable i <$ v1) (S.FunctionType (S.FunctionTypeN ([] <$ v1) t <$ t) <$ t) <$ v1) ([] <$ v1) <$ v1) <$ v1) t <$ v
        | otherwise                 -> v
      S.Application a
        | S.ApplicationN v vs <- copoint a ->
            let
              v' = term tids v
              vs' = term tids <$> copoint vs
            in S.TypedValue (S.Application (S.ApplicationN v' (vs' <$ vs) <$ a) <$ v1) t <$ v
      S.Procedure ps ->
        let
          p :| ps' = copoint ps
          (tids', p') = procedureStep tids p
          (_, ps'') = foldr go (tids', []) ps'
          go p (tids, ps) =
            let (tids', p') = procedureStep tids p
            in (tids', p':ps)
        in S.TypedValue (S.Procedure (p' :| ps'' <$ ps) <$ v1) t <$ v
      S.Let ds v ->
        let
          tids' = tids S.\\ identifiers ds
          ds'' = fmap (definition' tids') <$> ds
          v' = term tids' v
        in S.TypedValue (S.Let ds'' v' <$ v1) t <$ v
      _ -> v

procedureStep :: (Functor f, Copointed f, S.HasLocation f) => Set S.QualifiedIdentifier -> f (ProcedureStep f) -> (Set S.QualifiedIdentifier, f (ProcedureStep f))
procedureStep tids p =
  case copoint p of
    S.BindProcedureStep i v -> (tids S.\\ S.singleton (copoint i), S.BindProcedureStep i (term tids v) <$ p)
    S.CallProcedureStep v   ->   (tids, S.CallProcedureStep (term tids v) <$ p)

identifiers :: Copointed f => f [f (Definition f)] -> Set S.QualifiedIdentifier
identifiers =
  S.fromList . mapMaybe (go . copoint) . copoint
  where
    go (S.ValueBind b) | S.ValueBindN i _ _ <- copoint b = Just $ copoint i
    go _                                  = Nothing

thunkIdentifiers :: Copointed f => f [f (Definition f)] -> Set S.QualifiedIdentifier
thunkIdentifiers =
  S.fromList . mapMaybe (go . copoint) . copoint
  where
    go (S.ValueBind b) | S.ValueBindV i _ <- copoint b = Just $ copoint i
    go _                                = Nothing
