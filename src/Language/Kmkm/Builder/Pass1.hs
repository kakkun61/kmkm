{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Builder.Pass1
  ( convert
  ) where

import           Language.Kmkm.Exception     (Exception (Exception))
import qualified Language.Kmkm.Syntax        as S
import qualified Language.Kmkm.Syntax.Phase1 as P1
import qualified Language.Kmkm.Syntax.Phase2 as P2
import qualified Language.Kmkm.Syntax.Type   as T
import           Language.Kmkm.Syntax.Value  (Literal (Fraction, Function', Integer, String))
import qualified Language.Kmkm.Syntax.Value  as V

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Coerce         (Coercible, coerce)

convert :: MonadThrow m => P1.Module -> m P2.Module
convert = module'

module' :: MonadThrow m => P1.Module -> m P2.Module
module' (S.Module i ms) = S.Module i <$> sequence (member <$> ms)

member :: MonadThrow m => P1.Member -> m P2.Member
member (S.Definition i cs) =
  S.Definition i <$> sequence (go <$> cs)
  where
    go (i, fs) =
      (,) i <$> sequence (go <$> fs)
      where
        go (i, t) = (,) i <$> typ t
member (S.Bind b) = S.Bind <$> bind b

bind :: MonadThrow m => P1.Bind -> m P2.Bind
bind (S.Type i t)   = S.Type i <$> typ t
bind (S.Term i v t) = S.Term i <$> term' v <*> typ t

typ :: MonadThrow m => P1.Type -> m P2.Type
typ (T.Variable i)      = pure $ T.Variable i
typ (T.Application t s) = T.Application <$> typ t <*> typ s
typ (T.Arrow' a)        = T.Arrow' <$> arrow a

term :: (MonadThrow m, Coercible a P1.Term, Coercible P2.Term b) => a -> m b
term = (coerce <$>) . term' . coerce

term' :: MonadThrow m => P1.Term' -> m P2.Term'
term' (V.Variable i)      = pure $ V.Variable i
term' (V.Literal l)       = V.Literal <$> literal l
term' (V.Application' (P1.Application (V.Application (P1.Term (V.Application' (P1.Application (V.Application (P1.Term (V.Application' (P1.Application (V.Application (P1.Term (V.Application' _)) _)))) _)))) _))) =
  throwM $ Exception "function arity > 3"
term' (V.Application' (P1.Application (V.Application (P1.Term (V.Application' (P1.Application (V.Application (P1.Term (V.Application' (P1.Application (V.Application t t0)))) t1)))) t2))) =
  V.Application' . P2.Application <$> (V.Application3 <$> term t <*> term t0 <*> term t1 <*> term t2)
term' (V.Application' (P1.Application (V.Application (P1.Term (V.Application' (P1.Application (V.Application t t0)))) t1))) =
  V.Application' . P2.Application <$> (V.Application2 <$> term t <*> term t0 <*> term t1)
term' (V.Application' (P1.Application (V.Application t t0))) =
  V.Application' . P2.Application <$> (V.Application1 <$> term t <*> term t0)

literal :: MonadThrow m => P1.Literal -> m P2.Literal
literal (Integer v b)      = pure $ Integer v b
literal (Fraction s f e b) = pure $ Fraction s f e b
literal (String t)         = pure $ String t
literal (Function' f)      = Function' <$> function f

arrow :: MonadThrow m => T.Arrow -> m T.ArrowN
arrow (T.Arrow _ (T.Arrow' (T.Arrow _ (T.Arrow' (T.Arrow _ (T.Arrow' T.Arrow {})))))) = throwM $ Exception "function arity > 3"
arrow (T.Arrow t0 (T.Arrow' (T.Arrow t1 (T.Arrow' (T.Arrow t2 t))))) = T.Arrow3 <$> typ t0 <*> typ t1 <*> typ t2 <*> typ t
arrow (T.Arrow t0 (T.Arrow' (T.Arrow t1 t))) = T.Arrow2 <$> typ t0 <*> typ t1 <*> typ t
arrow (T.Arrow t0 t) = T.Arrow1 <$> typ t0 <*> typ t

function :: (MonadThrow m, Coercible a P1.Function, Coercible P2.Function b) => a -> m b
function = (coerce <$>) . function' . coerce

function' :: MonadThrow m => P1.Function' -> m P2.Function'
function' (V.Function _ (V.Literal (V.Function' (V.Function _ (V.Literal (Function' (V.Function _ (V.Literal Function' {})))))))) = throwM $ Exception "function arity > 3"
function' (V.Function i0 (V.Literal (Function' (V.Function i1 (V.Literal (Function' (V.Function i2 t))))))) = V.Function3 i0 i1 i2 <$> term t
function' (V.Function i0 (V.Literal (Function' (V.Function i1 t)))) = V.Function2 i0 i1 <$> term t
function' (V.Function i0 t) = V.Function1 i0 <$> term t
