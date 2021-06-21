-- | “top-level thunk / compile-time expression” pass.
module Language.Kmkm.Builder.C.Pass1
  ( thunk
  ) where

import           Language.Kmkm.Syntax        (Identifier, ModuleName, QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax        as S
import qualified Language.Kmkm.Syntax.Phase5 as P5
import qualified Language.Kmkm.Syntax.Phase6 as P6

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S

-- XXX: smarter method wanted
-- It is not necessary to convert all top-level value definitions,
-- but to do non-compile-time expressions only.
-- I think that a smarter method needs topological sort for dependencies
-- of value definitions.

thunk :: P5.Module -> P6.Module
thunk (S.Module n ds ms) = S.Module n ds $ definition n (thunkIdentifiers ms) <$> ms

definition :: ModuleName -> Set Identifier -> P5.Definition -> P6.Definition
definition n tids (S.ValueBind (S.BindV i v)) = definition' n tids (S.ValueBind (S.BindN i [] v))
definition n tids m                           = definition' n tids m

definition' :: ModuleName -> Set Identifier -> P5.Definition -> P6.Definition
definition' n tids (S.ValueBind (S.BindV i v))    = valueBind n tids i Nothing v
definition' n tids (S.ValueBind (S.BindN i ps v)) = valueBind n tids i (Just ps) v
definition' _ _ m                                 = m

valueBind :: ModuleName -> Set Identifier -> Identifier -> Maybe [(Identifier, P5.Type)] -> P5.Term -> P6.Definition
valueBind n tids i mps v =
  let
    v' = term n tids v
  in
    S.ValueBind (maybe (S.BindV i v') (\ps -> S.BindN i ps v') mps)

term :: ModuleName -> Set Identifier -> P5.Term -> P6.Term
term n tids v@(S.TypedTerm (S.Variable i@(QualifiedIdentifier n' i')) t)
  | Just n == n' && i' `S.member` tids = S.TypedTerm (S.Application $ S.ApplicationN (S.TypedTerm (S.Variable i) (S.FunctionType $ S.FunctionTypeN [] t)) []) t
  | otherwise                          = v
term n tids (S.TypedTerm (S.Application (S.ApplicationN v vs)) t) =
  S.TypedTerm (S.Application $ S.ApplicationN v' vs') t
  where
    v' = term n tids v
    vs' = term n tids <$> vs
term n tids (S.TypedTerm (S.Procedure (p :| ps)) t) =
  let
    (tids', p') = procedureStep n tids p
    (_, ps') = foldr go (tids', []) ps
    go p (tids, ps) =
      let (tids', p') = procedureStep n tids p
      in (tids', p':ps)
  in S.TypedTerm (S.Procedure $ p' :| ps') t
term n tids (S.TypedTerm (S.Let ds v) t) =
  let
    tids' = thunkIdentifiers ds `S.union` (tids S.\\ identifiers ds)
    ds' = definition' n tids' <$> ds
    v' = term n tids' v
  in S.TypedTerm (S.Let ds' v') t
term _ _ v = v

procedureStep :: ModuleName -> Set Identifier -> P5.ProcedureStep -> (Set Identifier, P6.ProcedureStep)
procedureStep n tids (S.BindProcedure i v) = (tids S.\\ S.singleton i, S.BindProcedure i $ term n tids v)
procedureStep n tids (S.TermProcedure v)   = (tids, S.TermProcedure $ term n tids v)

identifiers :: [P5.Definition] -> Set Identifier
identifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.BindN i _ _)) = Just i
    go _                             = Nothing

thunkIdentifiers :: [P5.Definition] -> Set Identifier
thunkIdentifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.BindV i _)) = Just i
    go S.ValueBind {}              = Nothing
    go _                           = Nothing
