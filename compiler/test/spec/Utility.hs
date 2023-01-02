{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Utility
  ( type I
  , pattern I
  , type FI
  , pattern FI
  ) where

import Data.Functor.F
import Data.Functor.Identity (Identity (Identity))
import GHC.Exts              (IsList (Item, fromList, toList))

instance IsList a => IsList (Identity a) where
  type Item (Identity a) = Item a
  fromList = Identity . fromList
  toList (Identity a) = toList a

type I = Identity

pattern I :: a -> Identity a
pattern I a = Identity a

type FI = F Identity

pattern FI :: a -> F Identity a
pattern FI a = F (Identity a)
