module Language.Kmkm.Internal.Build.C.Source
  ( source
  ) where

import qualified Language.Kmkm.Internal.Build.C.Syntax as I

import Data.Maybe (mapMaybe)

source :: I.File -> I.File
source (I.File n es) = I.File n $ mapMaybe (traverse element) es

element :: I.Element -> Maybe I.Element
element e@I.Definition {} = Just e
element _                 = Nothing
