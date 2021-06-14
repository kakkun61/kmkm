-- | \"top-level thunk / compile-time expression\" pass.
module Language.Kmkm.Builder.C.Pass1
  ( convert
  ) where

import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier)
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
convert (S.Module i ms) = S.Module i $ member (thunkIdentifiers ms) <$> ms

member :: Set Identifier -> P5.Member -> P6.Member
member tids (S.Bind (S.ValueBind (S.ValueBindV i v) ms)) =
  let
    tids' = (tids `S.difference` identifiers ms) `S.union` thunkIdentifiers ms
    v' = term tids' v
  in
    S.Bind $ S.ValueBind (S.ValueBindN i [] v') (member tids' <$> ms)
member _ m = m

term :: Set Identifier -> P5.Term -> P6.Term
term tids v@(V.TypedTerm (V.Variable i) t)
  | i `S.member` tids = V.TypedTerm (V.Application $ V.ApplicationN (V.TypedTerm (V.Variable i) (T.Function $ T.FunctionN [] t)) []) t
  | otherwise = v
term tids (V.TypedTerm (V.Application (V.ApplicationN v vs)) t) =
  V.TypedTerm (V.Application $ V.ApplicationN v' vs') t
  where
    v' = term tids v
    vs' = term tids <$> vs
term tids (V.TypedTerm (V.Procedure (p:|ps)) t) =
  let
    (tids', p') = procedureStep tids p
    (_, ps') = foldr go (tids', []) ps
    go p (tids, ps) =
      let (tids', p') = procedureStep tids p
      in (tids', p':ps)
  in V.TypedTerm (V.Procedure $ p':|ps') t
term _ v = v

procedureStep :: Set Identifier -> P5.ProcedureStep -> (Set Identifier, P6.ProcedureStep)
procedureStep tids (V.BindProcedure i v) = (tids `S.difference` S.singleton i, V.BindProcedure i $ term tids v)
procedureStep tids (V.TermProcedure v)   = (tids, V.TermProcedure $ term tids v)

identifiers :: [P5.Member] -> Set Identifier
identifiers =
  S.fromList . mapMaybe go
  where
    go (S.Bind (S.ValueBind (S.ValueBindN i _ _) _)) = Just i
    go S.Bind {}                                     = Nothing
    go _                                             = Nothing

thunkIdentifiers :: [P5.Member] -> Set Identifier
thunkIdentifiers =
  S.fromList . mapMaybe go
  where
    go (S.Bind (S.ValueBind (S.ValueBindV i _) _)) = Just i
    go (S.Bind S.ValueBind {})                     = Nothing
    go S.Bind {}                                   = Nothing
    go _                                           = Nothing
