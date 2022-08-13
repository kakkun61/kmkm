{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.CSpec where

import           Language.Kmkm.Internal.Build.C.C
import qualified Language.Kmkm.Internal.Build.C.Syntax as C

import Test.Hspec

spec :: Spec
spec = do
  describe "field" $ do
    it "void *foo" $ do
      field (C.Field ([], C.Void) (C.Identifier "item") [C.Pointer []])
        `shouldBe`
          "void (* item)"

  describe "derivers" $ do
    it "(*)" $ do
      derivers "" [C.Pointer[]] `shouldBe` "(*)"

    it "foo()" $ do
      derivers "foo" [C.Function []] `shouldBe` "foo()"

    it "foo(struct bar)" $ do
      derivers "foo" [C.Function [(([], C.Structure "bar"), [], Nothing, [])]] `shouldBe` "foo(struct bar)"

    it "(*)(struct bar)" $ do
      derivers "" [C.Pointer [], C.Function [(([], C.Structure "bar"), [], Nothing, [])]] `shouldBe` "(*)(struct bar)"

    it "(* foo)(struct bar)" $ do
      derivers "foo" [C.Pointer [], C.Function [(([], C.Structure "bar"), [], Nothing, [])]] `shouldBe` "(* foo)(struct bar)"
