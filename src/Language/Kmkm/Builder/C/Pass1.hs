-- | \"top-level thunk / compile-time expression\" pass.
module Language.Kmkm.Builder.C.Pass1
  ( convert
  ) where

import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier, ModuleName, QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax.Phase5 as P5
import qualified Language.Kmkm.Syntax.Phase6 as P6
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S

-- XXX: smarter method wanted
-- It is not necessary to convert all top-level value definitions,
-- but to do non-compile-time expressions only.
-- I think that a smarter method needs topological sort for dependencies
-- of value definitions.

convert :: P5.Module -> P6.Module
convert (S.Module n ds ms) = S.Module n ds $ member n (thunkIdentifiers ms) <$> ms

member :: ModuleName -> Set Identifier -> P5.Member -> P6.Member
member n tids (S.ValueBind (S.ValueBindV i v) ms) = member' n tids (S.ValueBind (S.ValueBindN i [] v) ms)
member n tids m = member' n tids m

member' :: ModuleName -> Set Identifier -> P5.Member -> P6.Member
member' n tids (S.ValueBind (S.ValueBindV i v) ms) = valueBind n tids i Nothing v ms
member' n tids (S.ValueBind (S.ValueBindN i ps v) ms) = valueBind n tids i (Just ps) v ms
member' _ _ m = m

valueBind :: ModuleName -> Set Identifier -> Identifier -> Maybe [(Identifier, P5.Type)] -> P5.Term -> [P5.Member] -> P6.Member
valueBind n tids i mps v ms =
  let
    tids' = tids `S.difference` identifiers ms
    v' = term n tids' v
  in
    S.ValueBind (maybe (S.ValueBindV i v') (\ps -> S.ValueBindN i ps v') mps) $ member' n tids' <$> ms

term :: ModuleName -> Set Identifier -> P5.Term -> P6.Term
term n tids v@(V.TypedTerm (V.Variable i@(QualifiedIdentifier n' i')) t)
  | Just n == n' &&i' `S.member` tids = V.TypedTerm (V.Application $ V.ApplicationN (V.TypedTerm (V.Variable i) (T.Function $ T.FunctionN [] t)) []) t
  | otherwise                         = v
term n tids (V.TypedTerm (V.Application (V.ApplicationN v vs)) t) =
  V.TypedTerm (V.Application $ V.ApplicationN v' vs') t
  where
    v' = term n tids v
    vs' = term n tids <$> vs
term n tids (V.TypedTerm (V.Procedure (p :| ps)) t) =
  let
    (tids', p') = procedureStep n tids p
    (_, ps') = foldr go (tids', []) ps
    go p (tids, ps) =
      let (tids', p') = procedureStep n tids p
      in (tids', p':ps)
  in V.TypedTerm (V.Procedure $ p' :| ps') t
term _ _ v = v

procedureStep :: ModuleName -> Set Identifier -> P5.ProcedureStep -> (Set Identifier, P6.ProcedureStep)
procedureStep n tids (V.BindProcedure i v) = (tids `S.difference` S.singleton i, V.BindProcedure i $ term n tids v)
procedureStep n tids (V.TermProcedure v)   = (tids, V.TermProcedure $ term n tids v)

identifiers :: [P5.Member] -> Set Identifier
identifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.ValueBindN i _ _) _) = Just i
    go _                                    = Nothing

thunkIdentifiers :: [P5.Member] -> Set Identifier
thunkIdentifiers =
  S.fromList . mapMaybe go
  where
    go (S.ValueBind (S.ValueBindV i _) _) = Just i
    go S.ValueBind {}                     = Nothing
    go _                                  = Nothing
