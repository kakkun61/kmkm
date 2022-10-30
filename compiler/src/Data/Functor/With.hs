{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}

module Data.Functor.With
  ( WithT (..)
  , With
  , pattern With
  , getMeta
  , getItem
  , Has (..)
  , MayHave (..)
  ) where

import Data.Copointed        (Copointed (copoint))
import Data.Functor.Classes  (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowsPrec))
import Data.Functor.F        (F, unf)
import Data.Functor.Identity (Identity (Identity))
import GHC.Generics          (Generic)

data WithT meta f item =
  WithT { meta :: meta, item :: f item }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)

getMeta :: WithT meta f item -> meta
getMeta (WithT meta _) = meta

getItem :: WithT meta f item -> f item
getItem (WithT _ item) = item

instance (Show meta, Show1 f) => Show1 (WithT meta f) where
  liftShowsPrec sp sl _ WithT { meta, item } =
    showString "WithT { meta = " . shows meta . showString ", item = " . liftShowsPrec sp sl 0 item . showString " }"

instance (Eq meta, Eq1 f) => Eq1 (WithT meta f) where
  liftEq eq WithT { meta = meta1, item = item1 } WithT { meta = meta2, item = item2 } =
    meta1 == meta2 && liftEq eq item1 item2

instance (Ord meta, Ord1 f) => Ord1 (WithT meta f) where
  liftCompare comp WithT { meta = meta1, item = item1 } WithT { meta = meta2, item = item2 } =
    compare meta1 meta2 <> liftCompare comp item1 item2

instance Copointed f => Copointed (WithT meta f) where
  copoint = copoint . getItem

type With meta = WithT meta Identity

pattern With :: meta -> item -> With meta item
pattern With meta item = WithT meta (Identity item)

{-# COMPLETE With #-}

class Has meta f where
  get :: f a -> meta

instance {-# OVERLAPPING #-} Has meta (WithT meta f) where
  get = getMeta

instance {-# OVERLAPPABLE #-} Has meta f => Has meta (WithT meta' f) where
  get = get . getItem

class MayHave meta f where
  mayGet :: f a -> Maybe meta
  default mayGet :: Has meta f => f a -> Maybe meta
  mayGet = Just . get

instance MayHave meta Identity where
  mayGet _ = Nothing

instance MayHave meta f => MayHave meta (F f) where
  mayGet = mayGet . unf

instance {-# OVERLAPPING #-} MayHave meta (WithT meta f) where
  mayGet = Just . getMeta

instance {-# OVERLAPPABLE #-} MayHave meta f => MayHave meta (WithT meta' f) where
  mayGet = mayGet . getItem
