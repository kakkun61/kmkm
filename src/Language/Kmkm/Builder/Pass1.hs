{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors      #-}
#else
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

-- | \"Type check\" pass.
module Language.Kmkm.Builder.Pass1
  ( typeCheck
  , Exception (..)
  ) where

import qualified Language.Kmkm.Exception     as X
import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier)
import qualified Language.Kmkm.Syntax.Phase1 as P1
import qualified Language.Kmkm.Syntax.Phase2 as P2
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

import qualified Control.Exception   as E
import           Control.Monad.Catch (MonadThrow (throwM))
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import qualified Data.Typeable       as Y
import           GHC.Generics        (Generic)

type Context = Map Identifier P1.Type

typeCheck :: MonadThrow m => P1.Module -> m P2.Module
typeCheck (S.Module i ms) = do
  ctx <- globalContext ms
  ms' <- typeCheck' ctx ms
  pure $ S.Module i ms'

globalContext :: MonadThrow m => [P1.Member] -> m Context
globalContext =
  foldr member $ pure M.empty
  where
    member :: MonadThrow m => P1.Member -> m Context -> m Context
    member (S.Definition i cs) =
      flip (foldr constructor) cs
      where
        constructor (c, fs) =
          (M.insert c (foldr field (T.Variable i) fs) <$>)
          where
            field (_, t) t' = T.Arrow $ T.ArrowC t t'
    member (S.Bind S.TypeBind {}) = id
    member (S.Bind (S.TermBind (S.TermBindUU i _ t))) = (M.insert i t <$>)

typeCheck' :: MonadThrow m => Context -> [P1.Member] -> m [P2.Member]
typeCheck' ctx =
  sequence . (member <$>)
  where
    member :: MonadThrow m => P1.Member -> m P2.Member
    member (S.Definition i cs) = pure $ S.Definition i cs
    member (S.Bind (S.TypeBind i t)) = pure $ S.Bind $ S.TypeBind i t
    member (S.Bind (S.TermBind (S.TermBindUU i (V.UntypedTerm v) t))) = do
        v'@(V.TypedTerm _ t') <- typeOf ctx v
        if t == t'
          then pure $ S.Bind $ S.TermBind $ S.TermBindUT i v'
          else throwM $ MismatchException (show t) $ show t'

typeOf :: MonadThrow m => Context -> P1.Term' -> m P2.Term
typeOf ctx (V.Variable i) =
  case M.lookup i ctx of
    Nothing -> throwM $ NotFoundException $ show i
    Just t  -> pure $ V.TypedTerm (V.Variable i) t
typeOf _ (V.Literal (V.Integer v b)) = pure $ V.TypedTerm (V.Literal (V.Integer v b)) (T.Variable "int")
typeOf _ (V.Literal (V.Fraction s d e b)) = pure $ V.TypedTerm (V.Literal (V.Fraction s d e b)) (T.Variable "frac2")
typeOf _ (V.Literal (V.String t)) = pure $ V.TypedTerm (V.Literal (V.String t)) (T.Variable "string")
typeOf ctx (V.Literal (V.Function (V.FunctionC i t (V.UntypedTerm v)))) = do
  v'@(V.TypedTerm _ t') <- typeOf (M.insert i t ctx) v
  pure $ V.TypedTerm (V.Literal (V.Function (V.FunctionC i t v'))) (T.Arrow $ T.ArrowC t t')
typeOf ctx (V.Application (V.ApplicationC (V.UntypedTerm v0) (V.UntypedTerm v1))) = do
  v0'@(V.TypedTerm _ t0) <- typeOf ctx v0
  v1'@(V.TypedTerm _ t1) <- typeOf ctx v1
  case t0 of
    T.Arrow (T.ArrowC t00 t01)
      | t1 == t00 -> pure $ V.TypedTerm (V.Application (V.ApplicationC v0' v1')) t01
      | otherwise -> throwM $ MismatchException (show t00) $ show t1
    _ -> throwM $ MismatchException "arrow" $ show t0

data Exception
  = NotFoundException String
  | MismatchException { expected :: String, actual :: String}
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
