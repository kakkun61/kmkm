{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Internal.Build.Uncurry
  ( uncurry
  ) where

import qualified Language.Kmkm.Internal.Syntax as S

import qualified Barbies.Bare       as B
import           Data.Copointed     (Copointed (copoint))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Prelude            (Functor (fmap, (<$)), flip, ($), (<$>))

type Module c et ev f = S.Module 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type Definition c  et ev f = S.Definition 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type Value c et ev f = S.Value 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type Value' c et ev f = S.Value' 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type ProcedureStep c et ev f = S.ProcedureStep 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type Application c et ev f = S.Application 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type Function c et ev f = S.Function 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed et ev B.Covered f

type Type c f = S.Type 'S.NameResolved c B.Covered f

type FunctionType c f = S.FunctionType 'S.NameResolved c B.Covered f

uncurry
  :: ( Functor f
     , Copointed f
     , S.HasLocation f
     )
  => f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed et ev B.Covered f)
  -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed et ev B.Covered f)
uncurry = module'

module' :: (Functor f, Copointed f, S.HasLocation f) => f (Module 'S.Curried et ev f) -> f (Module 'S.Uncurried et ev f)
module' m =
  go <$> m
  where
    go (S.Module i ds ms) = S.Module i ds $ fmap definition <$> ms

definition :: (Functor f, Copointed f, S.HasLocation f) => f (Definition 'S.Curried et ev f) -> f (Definition 'S.Uncurried et ev f)
definition =
  fmap $ \case
    S.DataDefinition i cs ->
      S.DataDefinition i $ fmap constructor <$> cs
      where
        constructor :: (Functor f, Copointed f, S.HasLocation f) => f (a, f [f (a1, f (Type 'S.Curried f))]) -> f (a, f [f (a1, f (Type 'S.Uncurried f))])
        constructor = fmap $ \case (i, fs) -> (i, fmap field <$> fs)
        field :: (Functor f, Copointed f, S.HasLocation f) => f (a, f (Type 'S.Curried f)) -> f (a, f (Type 'S.Uncurried f))
        field = fmap $ \case (i, t) -> (i, typ t)
    S.TypeBind i t -> S.TypeBind i $ typ t
    S.ForeignTypeBind i c -> S.ForeignTypeBind i c
    S.ValueBind (S.ValueBindU i v) -> S.ValueBind $ S.ValueBindU i $ typedValue v
    S.ForeignValueBind i c t -> S.ForeignValueBind i c $ typ t

typ :: (Functor f, Copointed f, S.HasLocation f) => f (Type 'S.Curried f) -> f (Type 'S.Uncurried f)
typ v =
  flip fmap v $ \case
    S.TypeVariable i      -> S.TypeVariable i
    S.TypeApplication t s -> S.TypeApplication (typ t) $ typ s
    S.FunctionType a      -> S.FunctionType $ functionType a
    S.ProcedureType t     -> S.ProcedureType $ typ t
    S.ForAllType i t      -> S.ForAllType i $ typ t

typedValue :: (Functor f, Copointed f, S.HasLocation f) => f (Value 'S.Curried et ev f) -> f (Value 'S.Uncurried et ev f)
typedValue = fmap $ \case S.TypedValue v t -> S.TypedValue (value' v) $ typ t

value' :: (Functor f, Copointed f, S.HasLocation f) => f (Value' 'S.Curried et ev f) -> f (Value' 'S.Uncurried et ev f)
value' v =
  flip fmap v $ \case
    S.Variable i    -> S.Variable i
    S.Literal l     -> S.Literal $ literal l
    S.Function f    -> S.Function $ function f v
    S.Application a -> S.Application $ application a v
    S.Procedure ps  -> S.Procedure $ ((procedureStep <$>) <$>) <$> ps
    S.Let ds v      -> S.Let (fmap definition <$> ds) $ typedValue v
    S.ForAll i v    -> S.ForAll i $ typedValue v

literal :: S.Literal -> S.Literal
literal (S.Integer v b)      = S.Integer v b
literal (S.Fraction s f e b) = S.Fraction s f e b
literal (S.String t)         = S.String t

functionType :: (Functor f, Copointed f, S.HasLocation f) => FunctionType 'S.Curried f -> FunctionType 'S.Uncurried f
functionType (S.FunctionTypeC t0 t) | S.FunctionType a <- copoint t =
  let
    S.FunctionTypeN ts t = functionType a
    t0' = typ t0
  in S.FunctionTypeN ((t0' :) <$> ts) t
functionType (S.FunctionTypeC t0 t) = S.FunctionTypeN ([typ t0] <$ t0) $ typ t

application :: (Functor f, Copointed f, S.HasLocation f) => Application 'S.Curried et ev f -> f a -> Application 'S.Uncurried et ev f
application a v =
  S.ApplicationN v' (vs <$ v)
  where
    v' :| vs = N.reverse $ go a
    go (S.ApplicationC v0 v1)
      | S.TypedValue v0' _ <- copoint v0
      , S.Application a <- copoint v0' = typedValue v1 :| N.toList (go a)
    go (S.ApplicationC v0 v1)                                = typedValue v1 :| [typedValue v0]

procedureStep :: (Functor f, Copointed f, S.HasLocation f) => ProcedureStep 'S.Curried et ev f -> ProcedureStep 'S.Uncurried et ev f
procedureStep (S.BindProcedureStep i v) = S.BindProcedureStep i $ typedValue v
procedureStep (S.CallProcedureStep v)   = S.CallProcedureStep $ typedValue v

function :: (Functor f, Copointed f, S.HasLocation f) => Function 'S.Curried et ev f -> f a -> Function 'S.Uncurried et ev f
function (S.FunctionC i t v') v
  | S.TypedValue v'' _ <- copoint v'
  , S.Function f <- copoint v'' =
  let
    t' = typ t
    S.FunctionN ps v''' = function f v''
  in S.FunctionN ((((i, t') <$ v) : copoint ps) <$ v) v'''
function (S.FunctionC i t v') v =
  let
    t' = typ t
    v'' = typedValue v'
  in S.FunctionN ([(i, t') <$ v] <$ v) v''
