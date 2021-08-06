module Language.Kmkm.Internal.Build.C.Header
  ( header
  ) where

import qualified Language.Kmkm.Internal.Build.C.Syntax as I

header :: I.File -> I.File
header (I.File n es) = I.File n $ fmap element <$> es

element :: I.Element -> I.Element
element (I.Definition (I.ExpressionDefinition t qs i ds _)) = I.Declaration t (I.External : qs) (Just i) ds
element (I.Definition (I.StatementDefinition t qs i ds _))  = I.Declaration t qs (Just i) ds
element e                                                   = e
