{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Builder.Pass2
  ( uncurry
  , Exception (..)
  ) where

import qualified Language.Kmkm.Exception     as X
import qualified Language.Kmkm.Syntax        as S
import qualified Language.Kmkm.Syntax.Phase2 as P2
import qualified Language.Kmkm.Syntax.Phase3 as P3
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

import qualified Control.Exception   as E
import           Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.Typeable       as Y
import           GHC.Generics        (Generic)
import           Prelude             (Applicative (pure, (<*>)), Eq, Ord, Read, Show, sequence, ($), (.), (<$>))

uncurry :: MonadThrow m => P2.Module -> m P3.Module
uncurry = module'

module' :: MonadThrow m => P2.Module -> m P3.Module
module' (S.Module i ms) = S.Module i <$> sequence (member <$> ms)

member :: MonadThrow m => P2.Member -> m P3.Member
member (S.Definition i cs) =
  S.Definition i <$> sequence (go <$> cs)
  where
    go (i, fs) =
      (,) i <$> sequence (go <$> fs)
      where
        go (i, t) = (,) i <$> typ t
member (S.Bind b) = S.Bind <$> bind b

bind :: MonadThrow m => P2.Bind -> m P3.Bind
bind (S.Type i t)   = S.Type i <$> typ t
bind (S.Term i v t) = S.Term i <$> term v <*> typ t

typ :: MonadThrow m => P2.Type -> m P3.Type
typ (T.Variable i)      = pure $ T.Variable i
typ (T.Application t s) = T.Application <$> typ t <*> typ s
typ (T.Arrow' a)        = T.Arrow' <$> arrow a

term :: MonadThrow m => P2.Term -> m P3.Term
term (V.TypedTerm (V.Variable i) t) = V.TypedTerm (V.Variable i) <$> typ t
term (V.TypedTerm (V.Literal l) t) = V.TypedTerm <$> (V.Literal <$> literal l) <*> typ t
term (V.TypedTerm (V.Application' (V.ApplicationC (V.TypedTerm (V.Application' (V.ApplicationC (V.TypedTerm (V.Application' (V.ApplicationC (V.TypedTerm V.Application' {} _) _)) _) _)) _) _)) _) =
  throwM TooLargeArityException
term (V.TypedTerm (V.Application' (V.ApplicationC (V.TypedTerm (V.Application' (V.ApplicationC (V.TypedTerm (V.Application' (V.ApplicationC i0 i1)) _) i2)) _) i3)) t) =
  V.TypedTerm <$> (V.Application' <$> (V.Application3 <$> term i0 <*> term i1 <*> term i2 <*> term i3)) <*> typ t
term (V.TypedTerm (V.Application' (V.ApplicationC (V.TypedTerm (V.Application' (V.ApplicationC i0 i1)) _) i2)) t) =
  V.TypedTerm <$> (V.Application' <$> (V.Application2 <$> term i0 <*> term i1 <*> term i2)) <*> typ t
term (V.TypedTerm (V.Application' (V.ApplicationC i0 i1)) t) =
  V.TypedTerm <$> (V.Application' <$> (V.Application1 <$> term i0 <*> term i1)) <*> typ t

literal :: MonadThrow m => P2.Literal -> m P3.Literal
literal (V.Integer v b)      = pure $ V.Integer v b
literal (V.Fraction s f e b) = pure $ V.Fraction s f e b
literal (V.String t)         = pure $ V.String t
literal (V.Function' f)      = V.Function' <$> function f

arrow :: MonadThrow m => P2.Arrow -> m P3.Arrow
arrow (T.ArrowC _ (T.Arrow' (T.ArrowC _ (T.Arrow' (T.ArrowC _ (T.Arrow' T.ArrowC {})))))) = throwM TooLargeArityException
arrow (T.ArrowC t0 (T.Arrow' (T.ArrowC t1 (T.Arrow' (T.ArrowC t2 t))))) = T.Arrow3 <$> typ t0 <*> typ t1 <*> typ t2 <*> typ t
arrow (T.ArrowC t0 (T.Arrow' (T.ArrowC t1 t))) = T.Arrow2 <$> typ t0 <*> typ t1 <*> typ t
arrow (T.ArrowC t0 t) = T.Arrow1 <$> typ t0 <*> typ t

function :: MonadThrow m => P2.Function -> m P3.Function
function (V.FunctionC _ _ (V.TypedTerm (V.Literal (V.Function' (V.FunctionC _ _ (V.TypedTerm (V.Literal (V.Function' (V.FunctionC _ _ (V.TypedTerm (V.Literal V.Function' {}) _)))) _)))) _)) =
  throwM TooLargeArityException
function (V.FunctionC i0 t0 (V.TypedTerm (V.Literal (V.Function' (V.FunctionC i1 t1 (V.TypedTerm (V.Literal (V.Function' (V.FunctionC i2 t2 v))) _)))) _)) =
  V.Function3 i0 <$> typ t0 <*> pure i1 <*> typ t1 <*> pure i2 <*> typ t2 <*> term v
function (V.FunctionC i0 t0 (V.TypedTerm (V.Literal (V.Function' (V.FunctionC i1 t1 v))) _)) =
  V.Function2 i0 <$> typ t0 <*> pure i1 <*> typ t1 <*> term v
function (V.FunctionC i0 t0 v) =
  V.Function1 i0 <$> typ t0 <*> term v

data Exception
  = TooLargeArityException
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
