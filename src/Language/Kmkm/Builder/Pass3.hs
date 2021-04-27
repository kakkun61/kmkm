-- | \"Partial application\" pass.
module Language.Kmkm.Builder.Pass3
  ( partialApplication
  ) where

import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier (SystemIdentifier))
import qualified Language.Kmkm.Syntax.Phase3 as P3
import qualified Language.Kmkm.Syntax.Phase4 as P4
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S

type Pass = State Word

partialApplication :: P3.Module -> P4.Module
partialApplication = flip evalState 0 . module'

module' :: P3.Module -> Pass P4.Module
module' (S.Module i ms) = S.Module i . (id =<<) <$> sequence (member <$> ms)

member :: P3.Member -> Pass [P4.Member]
member d@S.Definition {} = pure [d]
member (S.Bind b)        = (S.Bind <$>) <$> bind b

bind :: P3.Bind -> Pass [P4.Bind]
bind t@S.Type {} = pure [t]
bind (S.Term i v t) = do
  (v', bs) <- term v
  pure $ S.Term i v' t : bs

term :: P3.Term -> Pass (P4.Term, [P4.Bind])
term v@(V.TypedTerm V.Variable {} _) = pure (v, [])
term v@(V.TypedTerm V.Literal {} _) = pure (v, [])
term (V.TypedTerm (V.Application' (V.Application1 v0@(V.TypedTerm _ (T.Arrow' T.Arrow1 {})) v1)) t) =
  pure (V.TypedTerm (V.Application' $ V.Application1 v0 v1) t, [])
term (V.TypedTerm (V.Application' (V.Application1 v0@(V.TypedTerm _ (T.Arrow' (T.Arrow2 _ t01 t02))) v1)) t) = do
  a <- newIdentifier
  pure (V.TypedTerm (V.Literal $ V.Function' $ V.Function1 a t01 (V.TypedTerm (V.Application' $ V.Application2 v0 v1 $ V.TypedTerm (V.Variable a) t01) t02)) t, [])
term (V.TypedTerm (V.Application' (V.Application2 v0@(V.TypedTerm _ (T.Arrow' T.Arrow2 {})) v1 v2)) t) =
  pure (V.TypedTerm (V.Application' $ V.Application2 v0 v1 v2) t, [])
term (V.TypedTerm (V.Application' (V.Application3 v0@(V.TypedTerm _ (T.Arrow' T.Arrow3 {})) v1 v2 v3)) t) =
  pure (V.TypedTerm (V.Application' $ V.Application3 v0 v1 v2 v3) t, [])
term v = error $ show v

newIdentifier :: Pass Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SystemIdentifier n
