{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Instance () where

import Data.Functor.Identity (Identity (Identity))
import GHC.Exts              (IsList (Item, fromList, toList))

instance IsList a => IsList (Identity a) where
  type Item (Identity a) = Item a
  fromList = Identity . fromList
  toList (Identity a) = toList a

