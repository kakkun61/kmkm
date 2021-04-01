{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C
  ( build
  , buildC
  ) where

import qualified Language.Kmkm.Builder.C.Phase1 as P1
import qualified Language.Kmkm.Builder.C.Phase2 as P2

import           Language.Kmkm.Syntax       (Module (Module))
import Language.Kmkm.Syntax.Base ( Identifier(Identifier) )

import qualified Text.PrettyPrint      as Pretty
import Language.C (CTranslUnit)
import qualified Language.C.Pretty as C
import qualified Data.Text as Text

build :: Module -> Pretty.Doc
build m@(Module (Identifier i) _) =
  mconcat
    [ Pretty.text "#ifndef "
    , key
    , newline
    , Pretty.text "#define "
    , key
    , newline
    , C.pretty $ buildC m
    , newline
    , Pretty.text "#endif"
    , newline
    ]
  where
    key = Pretty.text $ Text.unpack $ Text.toUpper i <> "_H"
    newline = Pretty.char '\n'

buildC :: Module -> CTranslUnit
buildC =  P2.file . P1.module'
