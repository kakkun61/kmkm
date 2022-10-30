{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

-- | “top-level thunk / compile-time expression” pass.
module Language.Kmkm.Internal.Build.C.Thunk
  ( thunk
  ) where

import qualified Language.Kmkm.Internal.Syntax.Core.Common                                    as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaLifted as S

import           Data.Copointed     (Copointed (copoint))
import           Data.Functor.F     (F)
import           Data.Functor.With  (MayHave)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S

-- XXX: smarter method wanted
-- It is not necessary to convert all top-level value definitions,
-- but to do non-compile-time expressions only.
-- I think that a smarter method needs topological sort for dependencies
-- of value definitions.

type Definition = S.Definition S.EmbeddedCType S.EmbeddedCValue

type Value = S.Value S.EmbeddedCType S.EmbeddedCValue

type ProcedureStep = S.ProcedureStep S.EmbeddedCType S.EmbeddedCValue

thunk
  :: ( Functor f
     , Copointed f
     , MayHave SC.Location f
     )
  => F f (S.Module S.EmbeddedCType S.EmbeddedCValue f)
  -> F f (S.Module S.EmbeddedCType S.EmbeddedCValue f)
thunk = fmap $ \(S.Module n ms ds) -> S.Module n ms $ fmap (definition (thunkIdentifiers ds)) <$> ds

definition :: (Functor f, Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f (Definition f) -> F f (Definition f)
definition tids d =
  case copoint d of
    S.ValueBindV i is v -> definition' tids (S.ValueBindN i is ([] <$ d) v <$ d)
    _                   -> definition' tids d

definition' :: (Functor f, Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f (Definition f) -> F f (Definition f)
definition' tids =
  fmap $ \case
    S.ValueBindV i is v    -> valueBind tids i is Nothing v
    S.ValueBindN i is ps v -> valueBind tids i is (Just ps) v
    m                      -> m

valueBind :: (Functor f, Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f SC.QualifiedIdentifier -> F f [F f SC.QualifiedIdentifier] -> Maybe (F f [F f (F f SC.QualifiedIdentifier, F f (S.Type f))]) -> F f (Value f) -> Definition f
valueBind tids i is mps v =
  let v' = term tids v
  in maybe (S.ValueBindV i is v') (\ps -> S.ValueBindN i is ps v') mps

term :: (Functor f, Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f (Value f) -> F f (Value f)
term tids v =
  let S.TypedValue v1 t = copoint v
  in
    case copoint v1 of
      S.Variable i
        | copoint i `S.member` tids -> S.TypedValue (S.Application (S.TypedValue (S.Variable i <$ v1) (S.FunctionType ([] <$ v1) t <$ t) <$ v1) ([] <$ v1) <$ v1) t <$ v
        | otherwise                 -> v
      S.Application v vs ->
        let
          v' = term tids v
          vs' = term tids <$> copoint vs
        in S.TypedValue (S.Application v' (vs' <$ vs) <$ v1) t <$ v
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

procedureStep :: (Functor f, Copointed f, MayHave SC.Location f) => Set SC.QualifiedIdentifier -> F f (ProcedureStep f) -> (Set SC.QualifiedIdentifier, F f (ProcedureStep f))
procedureStep tids p =
  case copoint p of
    S.BindProcedureStep i v -> (tids S.\\ S.singleton (copoint i), S.BindProcedureStep i (term tids v) <$ p)
    S.CallProcedureStep v   ->   (tids, S.CallProcedureStep (term tids v) <$ p)

identifiers :: Copointed f => F f [F f (Definition f)] -> Set SC.QualifiedIdentifier
identifiers =
  S.fromList . mapMaybe (go . copoint) . copoint
  where
    go (S.ValueBindN i _ _ _) = Just $ copoint i
    go _                      = Nothing

thunkIdentifiers :: Copointed f => F f [F f (Definition f)] -> Set SC.QualifiedIdentifier
thunkIdentifiers =
  S.fromList . mapMaybe (go . copoint) . copoint
  where
    go (S.ValueBindV i _ _) = Just $ copoint i
    go _                    = Nothing
