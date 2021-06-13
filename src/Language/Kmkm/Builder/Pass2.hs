{-# LANGUAGE ApplicativeDo         #-}
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

import qualified Control.Exception  as E
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import qualified Data.Typeable      as Y
import           GHC.Generics       (Generic)
import           Prelude            (Applicative (pure, (<*>)), Eq, Ord, Read, Show, Traversable (sequenceA), ($), (.),
                                     (<$>))

uncurry :: Applicative m => P2.Module -> m P3.Module
uncurry = module'

module' :: Applicative m => P2.Module -> m P3.Module
module' (S.Module i ms) = S.Module i <$> sequenceA (member <$> ms)

member :: Applicative m => P2.Member -> m P3.Member
member (S.Definition i cs) =
  S.Definition i <$> sequenceA (go <$> cs)
  where
    go (i, fs) =
      (,) i <$> sequenceA (go <$> fs)
      where
        go (i, t) = (,) i <$> typ t
member (S.Bind b) = S.Bind <$> bind b

bind :: Applicative m => P2.Bind -> m P3.Bind
bind (S.TypeBind i t)                  = S.TypeBind i <$> typ t
bind (S.TermBind (S.TermBindU i v) ms) = S.TermBind <$> (S.TermBindU i <$> term v) <*> sequenceA (member <$> ms)

typ :: Applicative m => P2.Type -> m P3.Type
typ (T.Variable i)      = pure $ T.Variable i
typ (T.Application t s) = T.Application <$> typ t <*> typ s
typ (T.Function a)      = T.Function <$> arrow a
typ (T.Procedure t)     = T.Procedure <$> typ t

term :: Applicative m => P2.Term -> m P3.Term
term (V.TypedTerm (V.Variable i) t)       = V.TypedTerm (V.Variable i) <$> typ t
term (V.TypedTerm (V.Literal l) t)        = V.TypedTerm <$> (V.Literal <$> literal l) <*> typ t
term (V.TypedTerm (V.Application a) t)    = V.TypedTerm <$> (V.Application <$> application a) <*> typ t
term (V.TypedTerm (V.Procedure ps) t)     = V.TypedTerm <$> (V.Procedure <$> sequenceA (procedureStep <$> ps)) <*> typ t
term (V.TypedTerm (V.TypeAnnotation _) _) = X.unreachable

literal :: Applicative m => P2.Literal -> m P3.Literal
literal (V.Integer v b)      = pure $ V.Integer v b
literal (V.Fraction s f e b) = pure $ V.Fraction s f e b
literal (V.String t)         = pure $ V.String t
literal (V.Function f)       = V.Function <$> function f

arrow :: Applicative m => P2.TFunction -> m P3.TFunction
arrow (T.FunctionC t0 (T.Function a)) = do
  ~(T.FunctionN ts t) <- arrow a
  t0' <- typ t0
  pure $ T.FunctionN (t0' : ts) t
arrow (T.FunctionC t0 t) = T.FunctionN <$> ((:[]) <$> typ t0) <*> typ t

application :: Applicative m => P2.Application -> m P3.Application
application a = do
  ~(v :| vs) <- N.reverse <$> go a
  pure $ V.ApplicationN v vs
  where
    go (V.ApplicationC (V.TypedTerm (V.Application a) _) v1) = (:|) <$> term v1 <*> (N.toList <$> go a)
    go (V.ApplicationC v0 v1)                                = (:|) <$> term v1 <*> ((: []) <$> term v0)

procedureStep :: Applicative m => P2.ProcedureStep -> m P3.ProcedureStep
procedureStep (V.BindProcedure i v) = V.BindProcedure i <$> term v
procedureStep (V.TermProcedure v)   = V.TermProcedure <$> term v

function :: Applicative m => P2.Function -> m P3.Function
function (V.FunctionC i t (V.TypedTerm (V.Literal (V.Function f)) _)) = do
  t' <- typ t
  ~(V.FunctionN ps v) <- function f
  pure $ V.FunctionN ((i, t') : ps) v
function (V.FunctionC i t v) = do
  t' <- typ t
  v' <- term v
  pure $ V.FunctionN [(i, t')] v'

data Exception
  = TooLargeArityException
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
