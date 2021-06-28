{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Primitive
  ( int
  , uint
  , byte
  , frac
  , frac2
  ) where

import Language.Kmkm.Syntax (QualifiedIdentifier (GlobalIdentifier))

int, uint, byte, frac, frac2 :: QualifiedIdentifier

int   = GlobalIdentifier ["kmkm", "prim"] "int"
uint  = GlobalIdentifier ["kmkm", "prim"] "uint"
byte  = GlobalIdentifier ["kmkm", "prim"] "byte"
frac  = GlobalIdentifier ["kmkm", "prim"] "frac"
frac2 = GlobalIdentifier ["kmkm", "prim"] "frac2"

