module Language.Kmkm.Builder.C.Pass3
  ( header
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I

import Data.Maybe (mapMaybe)

header :: I.File -> I.File
header (I.File n es) = I.File n $ mapMaybe element es

element :: I.Element -> Maybe I.Element
element (I.Definition (I.ExpressionDefinition t qs i ds _)) = Just $ I.Declaration t qs (Just i) ds
element (I.Definition (I.StatementDefinition t qs i ds _))  = Just $ I.Declaration t qs (Just i) ds
element I.Embed {}                                          = Nothing
element e                                                   = Just e
