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
module' (S.Module i ms) = S.Module i . (id =<<) <$> sequence (member <$> ms)

member :: P4.Member -> Pass [P5.Member]
member (S.Definition i cs) = pure [S.Definition i cs]
member (S.Bind b)          = (S.Bind <$>) <$> bind b

bind :: P4.Bind -> Pass [P5.Bind]
bind (S.TypeBind i t) = pure [S.TypeBind i t]
bind (S.TermBind b)   = (S.TermBind <$>) <$> termBind b

termBind :: P4.TermBind -> Pass [P5.TermBind]
termBind (S.TermBindUT i (V.TypedTerm (V.Literal (V.Function (V.Function1 i0 t0 v))) _)) = do
  (v', bs) <- term v
  pure $ S.TermBind1 i i0 t0 v' : bs
termBind (S.TermBindUT i (V.TypedTerm (V.Literal (V.Function (V.Function2 i0 t0 i1 t1 v))) _)) = do
  (v', bs) <- term v
  pure $ S.TermBind2 i i0 t0 i1 t1 v' : bs
termBind (S.TermBindUT i (V.TypedTerm (V.Literal (V.Function (V.Function3 i0 t0 i1 t1 i2 t2 v))) _)) = do
  (v', bs) <- term v
  pure $ S.TermBind3 i i0 t0 i1 t1 i2 t2 v' : bs
termBind (S.TermBindUT i v) = do
  (v', bs) <- term v
  pure $ S.TermBind0 i v' : bs

term :: P4.Term -> Pass (P5.Term, [P5.TermBind])
term (V.TypedTerm (V.Variable i) t) = pure (V.TypedTerm (V.Variable i) t, [])
term (V.TypedTerm (V.Literal l) t) = do
  (l, bs) <- literal l
  pure (V.TypedTerm l t, bs)
term (V.TypedTerm (V.Application (V.Application1 v0 v1)) t) = do
  (v0', bs0) <- term v0
  (v1', bs1) <- term v1
  pure (V.TypedTerm (V.Application (V.Application1 v0' v1')) t, mconcat [bs0, bs1])
term (V.TypedTerm (V.Application (V.Application2 v0 v1 v2)) t) = do
  (v0', bs0) <- term v0
  (v1', bs1) <- term v1
  (v2', bs2) <- term v2
  pure (V.TypedTerm (V.Application (V.Application2 v0' v1' v2')) t, mconcat [bs0, bs1, bs2])
term (V.TypedTerm (V.Application (V.Application3 v0 v1 v2 v3)) t) = do
  (v0', bs0) <- term v0
  (v1', bs1) <- term v1
  (v2', bs2) <- term v2
  (v3', bs3) <- term v3
  pure (V.TypedTerm (V.Application (V.Application3 v0' v1' v2' v3')) t, mconcat [bs0, bs1, bs2, bs3])

literal :: P4.Literal -> Pass (P5.Term', [P5.TermBind])
literal (V.Integer v b) = pure (V.Literal $ V.Integer v b, [])
literal (V.Fraction s f e b) = pure (V.Literal $ V.Fraction s f e b, [])
literal (V.String t) = pure (V.Literal $ V.String t, [])
literal (V.Function (V.Function1 i0 t0 v)) = do
  i <- newIdentifier
  (v', bs) <- term v
  let b = S.TermBind1 i i0 t0 v'
  pure (V.Variable i, b:bs)
literal (V.Function (V.Function2 i0 t0 i1 t1 v)) = do
  i <- newIdentifier
  (v', bs) <- term v
  let b = S.TermBind2 i i0 t0 i1 t1 v'
  pure (V.Variable i, b:bs)
literal (V.Function (V.Function3 i0 t0 i1 t1 i2 t2 v)) = do
  i <- newIdentifier
  (v', bs) <- term v
  let b = S.TermBind3 i i0 t0 i1 t1 i2 t2 v'
  pure (V.Variable i, b:bs)

newIdentifier :: Pass Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SystemIdentifier 'l' n
