module Language.Kmkm
  ( compile
  , Config (..)
  , TypeMap (..)
  ) where

import Language.Kmkm.Compile (compile)
import Language.Kmkm.Config  (Config (Config), TypeMap (TypeMap, byte, frac, frac2, int))
