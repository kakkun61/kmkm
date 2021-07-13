{-# LANGUAGE RankNTypes #-}
module Barbies.Bare.Layered
  ( -- * Bare values
    Wear
  , Bare
  , Covered
    -- * Covering and stripping
  , BareB (bstrip, bcover)
  , bstripFrom
  , bcoverWith
  ) where

import Barbies.Bare                (Bare, Covered, Wear)
import Control.Monad.Identity      (Identity)
import Data.Functor.Barbie.Layered (FunctorB (bmap))
import Data.Functor.Identity       (Identity (Identity, runIdentity))

-- | Class of Barbie-types defined using 'Wear' and can therefore
-- have 'Bare' versions. Must satisfy:
--
-- @
-- 'bcover' . 'bstrip' = 'id'
-- 'bstrip' . 'bcover' = 'id'
-- @
class FunctorB (b Covered) => BareB b where
  bstrip :: b Covered Identity -> b Bare Identity
  bcover :: b Bare Identity -> b Covered Identity

-- | Generalization of 'bstrip' to arbitrary functors.
bstripFrom :: (BareB b, Functor f) => (forall a . f a -> a) -> b Covered f -> b Bare Identity
bstripFrom f = bstrip . bmap (Identity . f)

-- | Generalization of 'bcover' to arbitrary functors.
bcoverWith :: (BareB b, Functor f) => (forall a . a -> f a) -> b Bare Identity -> b Covered f
bcoverWith f = bmap (f . runIdentity) . bcover
