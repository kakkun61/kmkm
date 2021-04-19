{-# LANGUAGE FlexibleContexts #-}

-- | \"Identifier callee\" pass.
module Language.Kmkm.Builder.Pass2
  ( convert
  ) where

import           Data.Coerce                 (Coercible, coerce)
import qualified Language.Kmkm.Syntax        as S
import qualified Language.Kmkm.Syntax.Phase2 as P2
import qualified Language.Kmkm.Syntax.Phase3 as P3
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

convert :: P2.Module -> P3.Module
convert = module'

module' :: P2.Module -> P3.Module
module' (S.Module i ms) = S.Module i $ member <$> ms

member :: P2.Member -> P3.Member
member (S.Definition i cs) = S.Definition i $ go <$> cs where go (i, fs) = (i, go <$> fs) where go (i, t) = (i, typ t)
member (S.Bind b)          = S.Bind $ bind b

bind :: P2.Bind -> P3.Bind
bind (S.Type i t)   = S.Type i $ typ t
bind (S.Term i v t) = S.Term i (term' v) $ typ t

term :: (Coercible a P2.Term, Coercible P3.Term b) => a -> b
term = coerce . term' . coerce

term' :: P2.Term' -> P3.Term
term' (V.Variable i)                             = V.Variable i
term' (V.Literal l)                              = V.Literal $ literal l
term' (V.Application' (P2.Application (V.Application1 (P2.Term (V.Variable i)) v0))) =
  V.Application' $ P3.Application $ V.Application1 i $ term v0
term' (V.Application' (P2.Application (V.Application2 (P2.Term (V.Variable i)) v0 v1))) =
  V.Application' $ P3.Application $ V.Application2 i (term v0) $ term v1
term' (V.Application' (P2.Application (V.Application3 (P2.Term (V.Variable i)) v0 v1 v2))) =
  V.Application' $ P3.Application $ V.Application3 i (term v0) (term v1) $ term v2
term' v                                          = error $ show v

literal :: V.Literal P2.Function -> V.Literal P3.Function
literal (V.Integer v b)      = V.Integer v b
literal (V.Fraction s f e b) = V.Fraction s f e b
literal (V.String t)         = V.String t
literal (V.Function' f)      = V.Function' $ function f

function :: (Coercible a P2.Function, Coercible P3.Function b) => a -> b
function = coerce . function' . coerce

function' :: P2.Function' -> P3.Function'
function' (V.Function1 i0 v)       = V.Function1 i0 $ term v
function' (V.Function2 i0 i1 v)    = V.Function2 i0 i1 $ term v
function' (V.Function3 i0 i1 i2 v) = V.Function3 i0 i1 i2 $ term v

typ :: P2.Type -> P3.Type
typ (T.Variable i)      = T.Variable i
typ (T.Application t s) = T.Application (typ t) (typ s)
typ (T.Arrow' a)        = T.Arrow' a
