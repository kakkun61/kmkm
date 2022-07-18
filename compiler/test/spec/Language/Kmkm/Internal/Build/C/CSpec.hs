{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.CSpec where

import           Language.Kmkm.Internal.Build.C.C
import qualified Language.Kmkm.Internal.Build.C.Syntax as C

import Test.Hspec

spec :: Spec
spec = do
  describe "field" $ do
    it "void *foo;" $ do
      field (C.Field ([], C.Void) (C.Identifier "item") [C.Pointer []])
        `shouldBe`
          "void (* item)"
