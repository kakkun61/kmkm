{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Internal.Build.Uncurry
  ( uncurry
  ) where

import qualified Language.Kmkm.Internal.Syntax.Core.Common                                      as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Curried.LambdaUnlifted   as S3
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted as S4

import           Data.Copointed     (Copointed (copoint))
import           Data.Functor.F     (F)
import           Data.Functor.With  (MayHave)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           GHC.Stack          (HasCallStack)
import           Prelude            (Functor (fmap, (<$)), flip, otherwise, ($), (<$>))
import qualified Prelude

uncurry
  :: ( Functor f
     , Copointed f
     , MayHave SC.Location f
     , HasCallStack
     )
  => F f (S3.Module et ev f)
  -> F f (S4.Module et ev f)
uncurry = fmap $ \(S3.Module i ds ms) -> S4.Module i ds $ fmap definition <$> ms

definition :: (Functor f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S3.Definition et ev f) -> F f (S4.Definition et ev f)
definition =
  fmap $ \case
    S3.DataDefinition i r     -> S4.DataDefinition i $ dataRepresentation r
    S3.TypeBind i t           -> S4.TypeBind i $ typ t
    S3.ForeignTypeBind i c    -> S4.ForeignTypeBind i c
    S3.ValueBind i v          -> S4.ValueBind i $ typedValue v
    S3.ForeignValueBind i c t -> S4.ForeignValueBind i c $ typ t

dataRepresentation :: (Functor f, Copointed f, MayHave SC.Location f) => F f (S3.DataRepresentation et ev f) -> F f (S4.DataRepresentation et ev f)
dataRepresentation r =
  flip fmap r $ \r' ->
    let (is, cs) = go r'
    in S4.ForAllData (is <$ r) cs
  where
    go (S3.ForAllData i r) =
      let (is, cs) = go $ copoint r
      in (i : is, cs)
    go (S3.ValueConstructorsData cs) = ([], fmap valueConstructor <$> cs)

valueConstructor :: (Functor f, Copointed f, MayHave SC.Location f) => F f (S3.ValueConstructor et ev f) -> F f (S4.ValueConstructor et ev f)
valueConstructor = fmap $ \(S3.ValueConstructor i fs) -> S4.ValueConstructor i (fmap field <$> fs)

field :: (Functor f, Copointed f, MayHave SC.Location f) => F f (S3.Field et ev f) -> F f (S4.Field et ev f)
field = fmap $ \(S3.Field i t) -> S4.Field i (typ t)

typ :: (Functor f, Copointed f, MayHave SC.Location f) => F f (S3.Type f) -> F f (S4.Type f)
typ =
  fmap $ \case
    S3.TypeVariable i      -> S4.TypeVariable i
    S3.TypeApplication t s -> S4.TypeApplication (typ t) $ typ s
    S3.FunctionType t0 t1  -> Prelude.uncurry S4.FunctionType $ functionType t0 t1
    S3.ProcedureType t     -> S4.ProcedureType $ typ t
    S3.ForAllType i t      -> S4.ForAllType i $ typ t

typedValue :: (Functor f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S3.Value et ev f) -> F f (S4.Value et ev f)
typedValue = fmap $ \case S3.TypedValue v t -> S4.TypedValue (value' v) $ typ t

value' :: (Functor f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S3.Value' et ev f) -> F f (S4.Value' et ev f)
value' =
  fmap $ \case
    S3.Variable i       -> S4.Variable i
    S3.Literal l        -> S4.Literal $ literal <$> l
    S3.Function i t v       -> Prelude.uncurry S4.Function $ function i t v
    S3.Application v0 v1    ->
      let
        v :| vs = N.reverse $ go v0 v1
        go v0 v1 =
          let S3.TypedValue v0' _ = copoint v0
          in
            case copoint v0' of
              S3.Application v0_ v1_ -> typedValue v1 :| N.toList (go v0_ v1_)
              _                      -> typedValue v1 :| [typedValue v0]
      in S4.Application v (vs <$ v1)
    S3.Procedure ps     -> S4.Procedure $ ((procedureStep <$>) <$>) <$> ps
    S3.Let ds v         -> S4.Let (fmap definition <$> ds) $ typedValue v
    S3.ForAllValue i v  -> S4.ForAllValue i $ typedValue v
    S3.Instantiation v t -> S4.Instantiation (typedValue v) $ typ t

literal :: S3.Literal -> S4.Literal
literal (S3.Integer v b)      = S4.Integer v b
literal (S3.Fraction s f e b) = S4.Fraction s f e b
literal (S3.String t)         = S4.String t

functionType :: (Functor f, Copointed f, MayHave SC.Location f, HasCallStack) => F f (S3.Type f) -> F f (S3.Type f) -> (F f [F f (S4.Type f)], F f (S4.Type f))
functionType t0 t1
  | S3.FunctionType t10 t11 <- copoint t1 =
      let
        (ts, t') = functionType t10 t11
        t0' = typ t0
      in ((t0' :) <$> ts, t')
  | otherwise = ([typ t0] <$ t0, typ t1)

procedureStep :: (Functor f, Copointed f, MayHave SC.Location f, HasCallStack) => S3.ProcedureStep et ev f -> S4.ProcedureStep et ev f
procedureStep (S3.BindProcedureStep i v) = S4.BindProcedureStep i $ typedValue v
procedureStep (S3.CallProcedureStep v)   = S4.CallProcedureStep $ typedValue v

function :: (Functor f, Copointed f, MayHave SC.Location f, HasCallStack) => F f SC.QualifiedIdentifier -> F f (S3.Type f) -> F f (S3.Value et ev f) -> (F f [F f (F f SC.QualifiedIdentifier, F f (S4.Type f))], F f (S4.Value et ev f))
function i t v =
  let
    S3.TypedValue v' _ = copoint v
    t' = typ t
  in
    case copoint v' of
      S3.Function i_ t_ v_ ->
        let (ps, v_') = function i_ t_ v_
        in ((((i, t') <$ i) : copoint ps) <$ i, v_')
      _ -> ([(i, t') <$ i] <$ i, typedValue v)
