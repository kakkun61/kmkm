{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C
  ( build
  , buildC
  ) where

import qualified Language.Kmkm.Builder.C.Pass1 as BC1
import qualified Language.Kmkm.Builder.C.Pass2 as BC2
import qualified Language.Kmkm.Builder.C.Pass3 as BC3
import qualified Language.Kmkm.Builder.Pass1   as B1
import qualified Language.Kmkm.Builder.Pass2   as B2
import qualified Language.Kmkm.Builder.Pass3   as B3
import qualified Language.Kmkm.Builder.Pass4   as B4
import           Language.Kmkm.Syntax.Phase1   (Module)

import qualified Language.Kmkm.Syntax      as S
import           Language.Kmkm.Syntax.Base (ModuleName (ModuleName))

import           Control.Monad.Catch (MonadThrow)
import qualified Data.Text           as Text
import           Language.C          (CTranslUnit)
import qualified Language.C.Pretty   as C
import qualified Text.PrettyPrint    as Pretty

build :: MonadThrow m => Module -> m Pretty.Doc
build m@(S.Module (ModuleName i) _) = do
  u <- buildC m
  pure $
    mconcat
      [ Pretty.text "#ifndef "
      , key
      , newline
      , Pretty.text "#define "
      , key
      , newline
      , C.pretty u
      , newline
      , Pretty.text "#endif"
      , newline
      ]
  where
    key = Pretty.text $ Text.unpack $ Text.toUpper i <> "_H"
    newline = Pretty.char '\n'

buildC :: MonadThrow m => Module -> m CTranslUnit
buildC m = BC3.convert . BC2.convert . BC1.convert . B4.lambdaLifting . B3.partialApplication <$> (B2.uncurry =<< B1.typeCheck m)
