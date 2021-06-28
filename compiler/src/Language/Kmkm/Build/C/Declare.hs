module Language.Kmkm.Build.C.Declare
  ( declare
  ) where

import qualified Language.Kmkm.Build.C.Syntax as I

import           Data.Maybe            (mapMaybe)
import qualified Language.C.Syntax.AST as C

declare :: I.File -> I.File
declare (I.File n es) = I.File n $ mapMaybe element es

element :: I.Element -> Maybe I.Element
element (I.Definition (I.ExpressionDefinition t qs i ds _)) = Just $ I.Declaration t qs (Just i) ds
element (I.Definition (I.StatementDefinition t qs i ds _))  = Just $ I.Declaration t qs (Just i) ds
element (I.Embedded (I.C extDecl))                          = I.Embedded . I.C <$> cExternalDeclaration extDecl
element e                                                   = Just e

cExternalDeclaration :: C.CExtDecl -> Maybe C.CExtDecl
cExternalDeclaration (C.CDeclExt (C.CDecl specs opts pos)) =
  Just $ C.CDeclExt $ C.CDecl specs (mapMaybe f opts) pos
  where
    f (Just d, _, _)  = Just (Just d, Nothing, Nothing)
    f (Nothing, _, _) = Nothing
cExternalDeclaration _ = Nothing
