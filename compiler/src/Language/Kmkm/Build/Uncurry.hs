{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | \"Uncurry\" pass.
module Language.Kmkm.Build.Uncurry
  ( uncurry
  ) where

import           Language.Kmkm.Exception (unreachable)
import qualified Language.Kmkm.Syntax    as S

import qualified Barbies.Bare       as B
import           Data.Copointed     (Copointed (copoint))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import           Prelude            (Functor (fmap, (<$)), flip, ($), (<$>))

type Module c f = S.Module 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Definition c f = S.Definition 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Value c f = S.Value 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Value' c f = S.Value' 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type ProcedureStep c f = S.ProcedureStep 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Literal c f = S.Literal 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Application c f = S.Application 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Function c f = S.Function 'S.NameResolved c 'S.LambdaUnlifted 'S.Typed B.Covered f

type Type c f = S.Type 'S.NameResolved c B.Covered f

type FunctionType c f = S.FunctionType 'S.NameResolved c B.Covered f

uncurry
  :: ( Functor f
     , Copointed f
     , S.HasPosition f
     )
  => f (S.Module 'S.NameResolved 'S.Curried 'S.LambdaUnlifted 'S.Typed B.Covered f)
  -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaUnlifted 'S.Typed B.Covered f)
uncurry = module'

module' :: (Functor f, Copointed f, S.HasPosition f) => f (Module 'S.Curried f) -> f (Module 'S.Uncurried f)
module' m =
  go <$> m
  where
    go (S.Module i ds ms) = S.Module i ds $ fmap definition <$> ms

definition :: (Functor f, Copointed f, S.HasPosition f) => f (Definition 'S.Curried f) -> f (Definition 'S.Uncurried f)
definition =
  fmap $ \case
    S.DataDefinition i cs ->
      S.DataDefinition i $ fmap constructor <$> cs
      where
        constructor :: (Functor f, Copointed f, S.HasPosition f) => f (a, f [f (a1, f (Type 'S.Curried f))]) -> f (a, f [f (a1, f (Type 'S.Uncurried f))])
        constructor = fmap $ \case (i, fs) -> (i, fmap field <$> fs)
        field :: (Functor f, Copointed f, S.HasPosition f) => f (a, f (Type 'S.Curried f)) -> f (a, f (Type 'S.Uncurried f))
        field = fmap $ \case (i, t) -> (i, typ t)
    S.TypeBind i t -> S.TypeBind i $ typ t
    S.ForeignTypeBind i hs c -> S.ForeignTypeBind i hs c
    S.ValueBind (S.ValueBindU i v) -> S.ValueBind $ S.ValueBindU i $ typedValue v
    S.ForeignValueBind i hs c t -> S.ForeignValueBind i hs c $ typ t

typ :: (Functor f, Copointed f, S.HasPosition f) => f (Type 'S.Curried f) -> f (Type 'S.Uncurried f)
typ v =
  flip fmap v $ \case
    S.TypeVariable i      -> S.TypeVariable i
    S.TypeApplication t s -> S.TypeApplication (typ t) $ typ s
    S.FunctionType a      -> S.FunctionType $ functionType a
    S.ProcedureType t     -> S.ProcedureType $ typ t

typedValue :: (Functor f, Copointed f, S.HasPosition f) => f (Value 'S.Curried f) -> f (Value 'S.Uncurried f)
typedValue = fmap $ \case S.TypedValue v t -> S.TypedValue (value' v) $ typ t

value' :: (Functor f, Copointed f, S.HasPosition f) => f (Value' 'S.Curried f) -> f (Value' 'S.Uncurried f)
value' v =
  flip fmap v $ \case
    S.Variable i       -> S.Variable i
    S.Literal l        -> S.Literal $ literal l v
    S.Application a    -> S.Application $ application a v
    S.Procedure ps     -> S.Procedure $ ((procedureStep <$>) <$>) <$> ps
    S.TypeAnnotation _ -> unreachable
    S.Let ds v         -> S.Let (fmap definition <$> ds) $ typedValue v

literal :: (Functor f, Copointed f, S.HasPosition f) => Literal 'S.Curried f -> f a -> Literal 'S.Uncurried f
literal (S.Integer v b) _      = S.Integer v b
literal (S.Fraction s f e b) _ = S.Fraction s f e b
literal (S.String t) _         = S.String t
literal (S.Function f) v       = S.Function $ function f v

functionType :: (Functor f, Copointed f, S.HasPosition f) => FunctionType 'S.Curried f -> FunctionType 'S.Uncurried f
functionType (S.FunctionTypeC t0 t) | S.FunctionType a <- copoint t =
  let
    S.FunctionTypeN ts t = functionType a
    t0' = typ t0
  in S.FunctionTypeN ((t0' :) <$> ts) t
functionType (S.FunctionTypeC t0 t) = S.FunctionTypeN ([typ t0] <$ t0) $ typ t

application :: (Functor f, Copointed f, S.HasPosition f) => Application 'S.Curried f -> f a -> Application 'S.Uncurried f
application a v =
  S.ApplicationN v' (vs <$ v)
  where
    v' :| vs = N.reverse $ go a
    go (S.ApplicationC v0 v1)
      | S.TypedValue v0' _ <- copoint v0
      , S.Application a <- copoint v0' = typedValue v1 :| N.toList (go a)
    go (S.ApplicationC v0 v1)                                = typedValue v1 :| [typedValue v0]

procedureStep :: (Functor f, Copointed f, S.HasPosition f) => ProcedureStep 'S.Curried f -> ProcedureStep 'S.Uncurried f
procedureStep (S.BindProcedure i v) = S.BindProcedure i $ typedValue v
procedureStep (S.TermProcedure v)   = S.TermProcedure $ typedValue v

function :: (Functor f, Copointed f, S.HasPosition f) => Function 'S.Curried f -> f a -> Function 'S.Uncurried f
function (S.FunctionC i t v') v
  | S.TypedValue v'' _ <- copoint v'
  , S.Literal (S.Function f) <- copoint v'' =
  let
    t' = typ t
    S.FunctionN ps v''' = function f v''
  in S.FunctionN ((((i, t') <$ v) : copoint ps) <$ v) v'''
function (S.FunctionC i t v') v =
  let
    t' = typ t
    v'' = typedValue v'
  in S.FunctionN ([(i, t') <$ v] <$ v) v''
