{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.CSpec where

import           Language.Kmkm.Internal.Build.C.C
import qualified Language.Kmkm.Internal.Build.C.Syntax as C

import Test.Hspec

spec :: Spec
spec = do
  describe "qualifiedType" $ do
    it "" $ do
      qualifiedType (C.QualifiedType [] $ C.TypeVariable "kmkm_prim_int")
        `shouldBe`
          "kmkm_prim_int"

  describe "field" $ do
    it "void *foo" $ do
      field (C.Field (C.QualifiedType [] $ C.Void) (C.Identifier "item") [C.Pointer [] []])
        `shouldBe`
          "void (* item)"

  describe "derivers" $ do
    it "(*)" $ do
      derivers "" [C.Pointer[] []] `shouldBe` "(*)"

    it "foo()" $ do
      derivers "foo" [C.Function []] `shouldBe` "foo()"

    it "foo(struct bar)" $ do
      derivers "foo" [C.Function [(C.QualifiedType [] $ C.Structure "bar", [], Nothing, [])]] `shouldBe` "foo(struct bar)"

    it "(*)(struct bar)" $ do
      derivers "" [C.Pointer [] [], C.Function [(C.QualifiedType [] $ C.Structure "bar", [], Nothing, [])]] `shouldBe` "(*)(struct bar)"

    it "(* foo)(struct bar)" $ do
      derivers "foo" [C.Pointer [] [], C.Function [(C.QualifiedType [] $ C.Structure "bar", [], Nothing, [])]] `shouldBe` "(* foo)(struct bar)"

    it "(* (*)(kmkm_prim_string *))" $ do
      derivers "" [C.Pointer [] [C.Pointer [] [], C.Function [("kmkm_prim_string", [C.Constant], Nothing, [C.Pointer [C.Constant] []])]]]
        `shouldBe`
          "(* (*)(kmkm_prim_string const (* const)))"

  describe "expression" $ do
    it "((int) foo)" $ do
      expression (C.Cast ("int", []) "foo") `shouldBe` "((int) foo)"

    it "((int (*)(int)) foo)" $ do
      expression (C.Cast ("int", [C.Pointer [] [], C.Function [("int", [], Nothing, [])]]) "foo") `shouldBe` "((int (*)(int)) foo)"

    it "((char (* (*)(char *))) foo)" $ do
      expression (C.Cast ("char", [C.Pointer [] [C.Pointer [] [], C.Function [("char", [], Nothing, [C.Pointer [] []])]]]) "foo")
        `shouldBe`
          "((char (* (*)(char (*)))) foo)"

  describe "element" $ do
    it "kmkm_prim_int spec_foo = 10;" $ do
      element (C.Definition (C.ExpressionDefinition "kmkm_prim_int" [] "spec_foo" [] (C.ExpressionInitializer $ Right $ C.Literal $ C.Integer 10 C.IntDecimal)))
        `shouldBe`
          "kmkm_prim_int spec_foo = 10;"

    it "kmkm_prim_int (* spec_foo)(kmkm_prim_int const) = spec_bar;" $ do
      element (C.Definition (C.ExpressionDefinition "kmkm_prim_int" [] "spec_foo" [C.Pointer [] [], C.Function [("kmkm_prim_int", [C.Constant], Nothing, [])]] (C.ExpressionInitializer $ Right $ C.Variable "spec_bar")))
        `shouldBe`
          "kmkm_prim_int (* spec_foo)(kmkm_prim_int const) = spec_bar;"
