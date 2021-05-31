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
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as N
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import qualified Data.Typeable       as Y
import           GHC.Generics        (Generic)

type Context = Map Identifier P1.Type

typeCheck :: MonadThrow m => P1.Module -> m P2.Module
typeCheck (S.Module i ms) = do
  ctx <- context ms
  ms' <- typeCheck' ctx ms
  pure $ S.Module i ms'

context :: MonadThrow m => [P1.Member] -> m Context
context =
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
    member (S.Bind (S.TermBind (S.TermBindUU i _ t) _)) = (M.insert i t <$>)

typeCheck' :: MonadThrow m => Context -> [P1.Member] -> m [P2.Member]
typeCheck' ctx =
  sequence . (member <$>)
  where
    member :: MonadThrow m => P1.Member -> m P2.Member
    member (S.Definition i cs) = pure $ S.Definition i cs
    member (S.Bind (S.TypeBind i t)) = pure $ S.Bind $ S.TypeBind i t
    member (S.Bind (S.TermBind (S.TermBindUU i (V.UntypedTerm v) t) ms)) = do
      ctx' <- M.union ctx <$> context ms
      v'@(V.TypedTerm _ t') <- typeOfTerm ctx' v
      ms' <- typeCheck' ctx' ms
      if t == t'
        then pure $ S.Bind $ S.TermBind (S.TermBindUT i v') ms'
        else throwM $ MismatchException (show t) $ show t'

typeOfTerm :: MonadThrow m => Context -> P1.Term' -> m P2.Term
typeOfTerm ctx (V.Variable i) =
  case M.lookup i ctx of
    Nothing -> throwM $ NotFoundException $ show i
    Just t  -> pure $ V.TypedTerm (V.Variable i) t
typeOfTerm _ (V.Literal (V.Integer v b)) = pure $ V.TypedTerm (V.Literal (V.Integer v b)) (T.Variable "int")
typeOfTerm _ (V.Literal (V.Fraction s d e b)) = pure $ V.TypedTerm (V.Literal (V.Fraction s d e b)) (T.Variable "frac2")
typeOfTerm _ (V.Literal (V.String t)) = pure $ V.TypedTerm (V.Literal (V.String t)) (T.Variable "string")
typeOfTerm ctx (V.Literal (V.Function (V.FunctionC i t (V.UntypedTerm v)))) = do
  v'@(V.TypedTerm _ t') <- typeOfTerm (M.insert i t ctx) v
  pure $ V.TypedTerm (V.Literal (V.Function (V.FunctionC i t v'))) (T.Arrow $ T.ArrowC t t')
typeOfTerm ctx (V.Application (V.ApplicationC (V.UntypedTerm v0) (V.UntypedTerm v1))) = do
  v0'@(V.TypedTerm _ t0) <- typeOfTerm ctx v0
  v1'@(V.TypedTerm _ t1) <- typeOfTerm ctx v1
  case t0 of
    T.Arrow (T.ArrowC t00 t01)
      | t1 == t00 -> pure $ V.TypedTerm (V.Application (V.ApplicationC v0' v1')) t01
      | otherwise -> throwM $ MismatchException (show t00) $ show t1
    _ -> throwM $ MismatchException "arrow" $ show t0
typeOfTerm ctx (V.Procedure (p:|ps)) = do
  (ctx', p') <- typeOfProcedure ctx p
  (_, ps') <- foldr go (pure (ctx', [])) ps
  let ps'' = p':|ps'
  case N.last ps'' of
    V.TermProcedure (V.TypedTerm _ t) -> pure $ V.TypedTerm (V.Procedure ps'') t
    V.BindProcedure {}                -> throwM BindProcedureEndException
  where
    go p acc = do
      (ctx, ps) <- acc
      (ctx', p') <- typeOfProcedure ctx p
      pure (ctx', p':ps)

typeOfProcedure :: MonadThrow m => Context -> P1.Procedure -> m (Context, P2.Procedure)
typeOfProcedure ctx (V.BindProcedure i (V.UntypedTerm v)) = do
  v'@(V.TypedTerm _ t) <- typeOfTerm ctx v
  let ctx' = M.insert i t ctx
  pure (ctx', V.BindProcedure i v')
typeOfProcedure ctx (V.TermProcedure (V.UntypedTerm v)) = do
  v' <- typeOfTerm ctx v
  pure (ctx, V.TermProcedure v')

data Exception
  = NotFoundException String
  | MismatchException { expected :: String, actual :: String}
  | BindProcedureEndException
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
