{-# LANGUAGE DataKinds #-}

-- | “top-level thunk / compile-time expression” pass.
module Language.Kmkm.Build.C.Thunk
  ( thunk
  ) where

import qualified Language.Kmkm.Syntax as S

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S

-- XXX: smarter method wanted
-- It is not necessary to convert all top-level value definitions,
-- but to do non-compile-time expressions only.
-- I think that a smarter method needs topological sort for dependencies
-- of value definitions.

type Definition f = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f

type Type f = S.Type 'S.NameResolved 'S.Uncurried f

type Value f = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f

type ProcedureStep f = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f

thunk :: S.HasPosition f => S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f -> S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f
thunk (S.Module n ms ds) = S.Module n ms $ fmap (definition (thunkIdentifiers $ S.item <$> ds)) <$> ds

definition :: S.HasPosition f => Set S.QualifiedIdentifier -> Definition f -> Definition f
definition tids (S.ValueBind (S.ValueBindV i v)) = definition' tids (S.ValueBind (S.ValueBindN i [] v))
definition tids m                                = definition' tids m

definition' :: S.HasPosition f => Set S.QualifiedIdentifier -> Definition f -> Definition f
definition' tids (S.ValueBind (S.ValueBindV i v))    = valueBind tids i Nothing v
definition' tids (S.ValueBind (S.ValueBindN i ps v)) = valueBind tids i (Just ps) v
definition' _ m                                      = m

valueBind :: S.HasPosition f => Set S.QualifiedIdentifier -> f S.QualifiedIdentifier -> Maybe [(f S.QualifiedIdentifier, f (Type f))] -> f (Value f) -> Definition f
valueBind tids i mps v =
  let
    v' = term tids v
  in
    S.ValueBind (maybe (S.ValueBindV i v') (\ps -> S.ValueBindN i ps v') mps)

term :: S.HasPosition f => Set S.QualifiedIdentifier -> f (Value f) -> f (Value f)
term tids v =
  let S.TypedValue v1 t = S.item v
  in
    case S.item v1 of
      S.Variable i
        | i `S.member` tids -> S.TypedValue (S.Application (S.ApplicationN (S.TypedValue (S.Variable i <$ v1) (S.FunctionType (S.FunctionTypeN [] t) <$ t) <$ v1) []) <$ v1) t <$ v
        | otherwise                          -> v
      S.Application (S.ApplicationN v vs) ->
        S.TypedValue (S.Application (S.ApplicationN v' vs') <$ v1) t <$ v
        where
          v' = term tids v
          vs' = term tids <$> vs
      S.Procedure (p :| ps) ->
        let
          (tids', p') = procedureStep tids p
          (_, ps') = foldr go (tids', []) ps
          go p (tids, ps) =
            let (tids', p') = procedureStep tids p
            in (tids', p':ps)
        in S.TypedValue (S.Procedure (p' :| ps') <$ v1) t <$ v
      S.Let ds v ->
        let
          ds' = S.item <$> ds
          tids' = thunkIdentifiers ds' `S.union` (tids S.\\ identifiers ds')
          ds'' = fmap (definition' tids') <$> ds
          v' = term tids' v
        in S.TypedValue (S.Let ds'' v' <$ v1) t <$ v
      _ -> v

procedureStep :: S.HasPosition f => Set S.QualifiedIdentifier -> f (ProcedureStep f) -> (Set S.QualifiedIdentifier, f (ProcedureStep f))
procedureStep tids p =
  case S.item p of
    S.BindProcedure i v -> (tids S.\\ S.singleton (S.item i), S.BindProcedure i (term tids v) <$ p)
    S.TermProcedure v   ->   (tids, S.TermProcedure (term tids v) <$ p)

identifiers :: S.HasPosition f => [Definition f] -> Set S.QualifiedIdentifier
identifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.ValueBindN i _ _)) = Just $ S.item i
    go _                                  = Nothing

thunkIdentifiers :: S.HasPosition f => [Definition f] -> Set S.QualifiedIdentifier
thunkIdentifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.ValueBindV i _)) = Just $ S.item i
    go S.ValueBind {}                   = Nothing
    go _                                = Nothing
