{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

module Data.Functor.With
  ( WithT (..)
  , With
  , pattern With
  , Has (..)
  , MayHave (..)
  ) where

import Data.Copointed        (Copointed (copoint))
import Data.Functor.Identity (Identity (Identity))
import GHC.Generics          (Generic)

data WithT meta f item =
  WithT { meta :: meta, item :: f item }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Copointed f => Copointed (WithT meta f) where
  copoint (WithT _ item) = copoint item

type With meta = WithT meta Identity

pattern With :: meta -> item -> With meta item
pattern With meta item = WithT meta (Identity item)

{-# COMPLETE With #-}

class Has meta f where
  get :: f a -> meta

instance {-# OVERLAPPING #-} Has meta (WithT meta f) where
  get (WithT meta _) = meta

instance {-# OVERLAPPABLE #-} Has meta f => Has meta (WithT meta' f) where
  get (WithT _ item) = get item

class MayHave meta f where
  mayGet :: f a -> Maybe meta
  default mayGet :: Has meta f => f a -> Maybe meta
  mayGet = Just . get

instance MayHave meta Identity where
  mayGet _ = Nothing

instance {-# OVERLAPPING #-} MayHave meta (WithT meta f) where
  mayGet (WithT meta _) = Just meta

instance {-# OVERLAPPABLE #-} MayHave meta f => MayHave meta (WithT meta' f) where
  mayGet (WithT _ item) = mayGet item
