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

import           Data.Maybe (mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as S

-- XXX: smarter method wanted
-- It is not necessary to convert all top-level value definitions,
-- but to do non-compile-time expressions only.
-- I think that a smarter method needs topological sort for dependencies
-- of value definitions.

convert :: P5.Module -> P6.Module
convert (S.Module i ms) = S.Module i $ member (thunkIdentifiers ms) <$> ms

member :: Set Identifier -> P5.Member -> P6.Member
member tids (S.Bind (S.TermBind (S.TermBindV i v) ms)) =
  let
    tids' = (tids `S.difference` identifiers ms) `S.union` thunkIdentifiers ms
    v' = term tids' v
  in
    S.Bind $ S.TermBind (S.TermBind0 i v') (member tids' <$> ms)
member _ m = m

term :: Set Identifier -> P5.Term -> P6.Term
term tids v@(V.TypedTerm (V.Variable i) t)
  | i `S.member` tids = V.TypedTerm (V.Application $ V.Application0 (V.TypedTerm (V.Variable i) (T.Arrow $ T.Arrow0 t))) t
  | otherwise = v
term tids (V.TypedTerm (V.Application (V.Application0 v)) t) =
  V.TypedTerm (V.Application $ V.Application0 v') t
  where
    v' = term tids v
term tids (V.TypedTerm (V.Application (V.Application1 v v1)) t) =
  V.TypedTerm (V.Application $ V.Application1 v' v1') t
  where
    v' = term tids v
    v1' = term tids v1
term tids (V.TypedTerm (V.Application (V.Application2 v v1 v2)) t) =
  V.TypedTerm (V.Application $ V.Application2 v' v1' v2') t
  where
    v' = term tids v
    v1' = term tids v1
    v2' = term tids v2
term tids (V.TypedTerm (V.Application (V.Application3 v v1 v2 v3)) t) =
  V.TypedTerm (V.Application $ V.Application3 v' v1' v2' v3') t
  where
    v' = term tids v
    v1' = term tids v1
    v2' = term tids v2
    v3' = term tids v3
term _ v = v

identifiers :: [P5.Member] -> Set Identifier
identifiers =
  S.fromList . mapMaybe go
  where
    go (S.Bind (S.TermBind (S.TermBindV i _) _))             = Just i
    go (S.Bind (S.TermBind (S.TermBind0 i _) _))             = Just i
    go (S.Bind (S.TermBind (S.TermBind1 i _ _ _) _))         = Just i
    go (S.Bind (S.TermBind (S.TermBind2 i _ _ _ _ _) _))     = Just i
    go (S.Bind (S.TermBind (S.TermBind3 i _ _ _ _ _ _ _) _)) = Just i
    go S.Bind {}                                             = Nothing
    go _                                                     = Nothing

thunkIdentifiers :: [P5.Member] -> Set Identifier
thunkIdentifiers =
  S.fromList . mapMaybe go
  where
    go (S.Bind (S.TermBind (S.TermBindV i _) _)) = Just i
    go (S.Bind S.TermBind {})                    = Nothing
    go S.Bind {}                                 = Nothing
    go _                                         = Nothing
