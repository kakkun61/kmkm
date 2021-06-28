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

type Module = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type Definition = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type Type = S.Type 'S.NameResolved 'S.Uncurried

type Value = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type ProcedureStep = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

thunk :: Module -> Module
thunk (S.Module n ds ms) = S.Module n ds $ definition (thunkIdentifiers ms) <$> ms

definition :: Set S.QualifiedIdentifier -> Definition -> Definition
definition tids (S.ValueBind (S.ValueBindV i v)) = definition' tids (S.ValueBind (S.ValueBindN i [] v))
definition tids m                                = definition' tids m

definition' :: Set S.QualifiedIdentifier -> Definition -> Definition
definition' tids (S.ValueBind (S.ValueBindV i v))    = valueBind tids i Nothing v
definition' tids (S.ValueBind (S.ValueBindN i ps v)) = valueBind tids i (Just ps) v
definition' _ m                                      = m

valueBind :: Set S.QualifiedIdentifier -> S.QualifiedIdentifier -> Maybe [(S.QualifiedIdentifier, Type)] -> Value -> Definition
valueBind tids i mps v =
  let
    v' = term tids v
  in
    S.ValueBind (maybe (S.ValueBindV i v') (\ps -> S.ValueBindN i ps v') mps)

term :: Set S.QualifiedIdentifier -> Value -> Value
term tids v@(S.TypedTerm (S.Variable i) t)
  | i `S.member` tids = S.TypedTerm (S.Application $ S.ApplicationN (S.TypedTerm (S.Variable i) (S.FunctionType $ S.FunctionTypeN [] t)) []) t
  | otherwise                          = v
term tids (S.TypedTerm (S.Application (S.ApplicationN v vs)) t) =
  S.TypedTerm (S.Application $ S.ApplicationN v' vs') t
  where
    v' = term tids v
    vs' = term tids <$> vs
term tids (S.TypedTerm (S.Procedure (p :| ps)) t) =
  let
    (tids', p') = procedureStep tids p
    (_, ps') = foldr go (tids', []) ps
    go p (tids, ps) =
      let (tids', p') = procedureStep tids p
      in (tids', p':ps)
  in S.TypedTerm (S.Procedure $ p' :| ps') t
term tids (S.TypedTerm (S.Let ds v) t) =
  let
    tids' = thunkIdentifiers ds `S.union` (tids S.\\ identifiers ds)
    ds' = definition' tids' <$> ds
    v' = term tids' v
  in S.TypedTerm (S.Let ds' v') t
term _ v = v

procedureStep :: Set S.QualifiedIdentifier -> ProcedureStep -> (Set S.QualifiedIdentifier, ProcedureStep)
procedureStep tids (S.BindProcedure i v) = (tids S.\\ S.singleton i, S.BindProcedure i $ term tids v)
procedureStep tids (S.TermProcedure v)   = (tids, S.TermProcedure $ term tids v)

identifiers :: [Definition] -> Set S.QualifiedIdentifier
identifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.ValueBindN i _ _)) = Just i
    go _                                  = Nothing

thunkIdentifiers :: [Definition] -> Set S.QualifiedIdentifier
thunkIdentifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.ValueBindV i _)) = Just i
    go S.ValueBind {}                   = Nothing
    go _                                = Nothing
