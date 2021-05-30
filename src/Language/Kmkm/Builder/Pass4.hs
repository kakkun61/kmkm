-- | \"Lambda lifting\" pass.
module Language.Kmkm.Builder.Pass4
  ( lambdaLifting
  ) where

import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier (SystemIdentifier))
import qualified Language.Kmkm.Syntax.Phase4 as P4
import qualified Language.Kmkm.Syntax.Phase5 as P5
import qualified Language.Kmkm.Syntax.Value  as V

import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S

type Pass = State Word

lambdaLifting :: P4.Module -> P5.Module
lambdaLifting = flip evalState 0 . module'

module' :: P4.Module -> Pass P5.Module
module' (S.Module i ms) = S.Module i <$> sequence (member <$> ms)

member :: P4.Member -> Pass P5.Member
member (S.Definition i cs) = pure $ S.Definition i cs
member (S.Bind b)          = S.Bind <$> bind b

bind :: P4.Bind -> Pass P5.Bind
bind (S.TypeBind i t) = pure $ S.TypeBind i t
bind (S.TermBind b ms) =
  scope $ do
    ms' <- sequence $ member <$> ms
    (b, ms'') <- termBind b
    pure $ S.TermBind b (ms' ++ ms'')

termBind :: P4.TermBind -> Pass (P5.TermBind, [P5.Member])
termBind (S.TermBindUT i (V.TypedTerm (V.Literal (V.Function (V.Function1 i0 t0 v))) _)) = do
  (v', ms) <- term v
  pure (S.TermBind1 i i0 t0 v', ms)
termBind (S.TermBindUT i (V.TypedTerm (V.Literal (V.Function (V.Function2 i0 t0 i1 t1 v))) _)) = do
  (v', ms) <- term v
  pure (S.TermBind2 i i0 t0 i1 t1 v', ms)
termBind (S.TermBindUT i (V.TypedTerm (V.Literal (V.Function (V.Function3 i0 t0 i1 t1 i2 t2 v))) _)) = do
  (v', ms) <- term v
  pure (S.TermBind3 i i0 t0 i1 t1 i2 t2 v', ms)
termBind (S.TermBindUT i v) = do
  (v', ms) <- term v
  pure (S.TermBindV i v', ms)

term :: P4.Term -> Pass (P5.Term, [P5.Member])
term (V.TypedTerm (V.Variable i) t) = pure (V.TypedTerm (V.Variable i) t, [])
term (V.TypedTerm (V.Literal l) t) = do
  (l, ms) <- literal l
  pure (V.TypedTerm l t, ms)
term (V.TypedTerm (V.Application (V.Application0 v0)) t) = do
  (v0', ms0) <- term v0
  pure (V.TypedTerm (V.Application (V.Application0 v0')) t, ms0)
term (V.TypedTerm (V.Application (V.Application1 v0 v1)) t) = do
  (v0', ms0) <- term v0
  (v1', ms1) <- term v1
  pure (V.TypedTerm (V.Application (V.Application1 v0' v1')) t, mconcat [ms0, ms1])
term (V.TypedTerm (V.Application (V.Application2 v0 v1 v2)) t) = do
  (v0', ms0) <- term v0
  (v1', ms1) <- term v1
  (v2', ms2) <- term v2
  pure (V.TypedTerm (V.Application (V.Application2 v0' v1' v2')) t, mconcat [ms0, ms1, ms2])
term (V.TypedTerm (V.Application (V.Application3 v0 v1 v2 v3)) t) = do
  (v0', ms0) <- term v0
  (v1', ms1) <- term v1
  (v2', ms2) <- term v2
  (v3', ms3) <- term v3
  pure (V.TypedTerm (V.Application (V.Application3 v0' v1' v2' v3')) t, mconcat [ms0, ms1, ms2, ms3])

literal :: P4.Literal -> Pass (P5.Term', [P5.Member])
literal (V.Integer v b) = pure (V.Literal $ V.Integer v b, [])
literal (V.Fraction s f e b) = pure (V.Literal $ V.Fraction s f e b, [])
literal (V.String t) = pure (V.Literal $ V.String t, [])
literal (V.Function (V.Function1 i0 t0 v)) = do
  i <- newIdentifier
  (v', ms) <- term v
  let m = S.Bind $ S.TermBind (S.TermBind1 i i0 t0 v') ms
  pure (V.Variable i, [m])
literal (V.Function (V.Function2 i0 t0 i1 t1 v)) = do
  i <- newIdentifier
  (v', ms) <- term v
  let m = S.Bind $ S.TermBind (S.TermBind2 i i0 t0 i1 t1 v') ms
  pure (V.Variable i, [m])
literal (V.Function (V.Function3 i0 t0 i1 t1 i2 t2 v)) = do
  i <- newIdentifier
  (v', ms) <- term v
  let m = S.Bind $ S.TermBind (S.TermBind3 i i0 t0 i1 t1 i2 t2 v') ms
  pure (V.Variable i, [m])

newIdentifier :: Pass Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SystemIdentifier 'l' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r
