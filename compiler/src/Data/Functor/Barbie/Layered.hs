{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Functor.Barbie.Layered
  ( FunctorB (..)
  ) where

import Data.Kind (Constraint, Type)

-- | Barbie-types that can be mapped over. Instances of 'FunctorB' should
-- satisfy the following laws:
--
-- @
-- 'bmap' 'id' = 'id'
-- 'bmap' f . 'bmap' g = 'bmap' (f . g)
-- @
type FunctorB :: ((Type-> Type) -> Type) -> Constraint
class FunctorB b where
  bmap :: forall f g. (Functor f, Functor g) => (forall a. f a -> g a) -> b f -> b g
