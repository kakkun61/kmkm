-- | \"Partial application\" pass.
module Language.Kmkm.Builder.Pass3
  ( partialApplication
  ) where

import           Language.Kmkm.Exception     (unreachable)
import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier (SystemIdentifier), ModuleName,
                                              QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax.Phase3 as P3
import qualified Language.Kmkm.Syntax.Phase4 as P4
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

import           Control.Monad              (replicateM)
import           Control.Monad.State.Strict (State, evalState)
import qualified Control.Monad.State.Strict as S

type Pass = State Word

partialApplication :: P3.Module -> P4.Module
partialApplication = flip evalState 0 . module'

module' :: P3.Module -> Pass P4.Module
module' (S.Module mn ds ms) = S.Module mn ds <$> sequence (member mn <$> ms)

member :: ModuleName -> P3.Member -> Pass P4.Member
member _ d@S.Definition {} = pure d
member _ t@S.TypeBind {} = pure t
member mn (S.ValueBind (S.ValueBindU i v) ms) =
  scope $ do
    v' <- term mn v
    ms' <- sequence $ member mn <$> ms
    pure $ S.ValueBind (S.ValueBindU i v') ms'
member _ b@S.ForeignValueBind {} = pure b

term :: ModuleName -> P3.Term -> Pass P4.Term
term mn (V.TypedTerm (V.Application (V.ApplicationN v@(V.TypedTerm _ (T.Function (T.FunctionN t0s t0))) vs)) t) = do
  let
    nApp = length vs
    nFun = length t0s
  if nFun < nApp
    then unreachable
    else do
      v' <- term mn v
      vs' <- sequence $ term mn <$> vs
      if nApp == nFun
        then
          pure $ V.TypedTerm (V.Application (V.ApplicationN v' vs')) t
        else do -- nApp < nFun
          let nCls = nFun - nApp
          is <- replicateM nCls newIdentifier
          let
            t0s' = drop nApp t0s
            vs'' = vs' ++ (V.TypedTerm . V.Variable <$> (QualifiedIdentifier Nothing <$> is) <*> t0s')
          pure $ V.TypedTerm (V.Literal $ V.Function $ V.FunctionN (zip is t0s') (V.TypedTerm (V.Application (V.ApplicationN v' vs'')) t0)) t
term mn (V.TypedTerm (V.Procedure ps) t) =
  flip V.TypedTerm t . V.Procedure <$> sequence (procedureStep mn <$> ps)
term _ v = pure v

procedureStep :: ModuleName -> P3.ProcedureStep -> Pass P4.ProcedureStep
procedureStep mn (V.BindProcedure i v) = V.BindProcedure i <$> term mn v
procedureStep mn (V.TermProcedure v)   = V.TermProcedure <$> term mn v

newIdentifier :: Pass Identifier
newIdentifier = do
  n <- S.get
  S.put $ n + 1
  pure $ SystemIdentifier 'a' n

scope :: Pass a -> Pass a
scope p = do
  n <- S.get
  r <- p
  S.put n
  pure r
