{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Build.C.IntermediateCSpec where

import           Language.Kmkm.Build.C.IntermediateC
import qualified Language.Kmkm.Build.C.Syntax        as C
import qualified Language.Kmkm.Syntax                as S

import Data.Default.Class
import Language.Kmkm.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "intermediate" $ do
    describe "definition" $ do
      describe "v" $ do
        it "foo :: int ⇒ int foo" $ do
          definition def "spec" (S.ValueBind (S.ValueBindV ["spec", "foo"] $ S.TypedTerm (S.Literal $ S.Integer 10 10) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldBe`
              [C.Definition (C.ExpressionDefinition ([], C.Int) [] "spec_foo" [] (C.ExpressionInitializer $ C.Literal $ C.Integer 10 C.IntDecimal))]

        it "foo :: int → int ⇒ int (*foo)(int)" $ do
          definition def "spec" (S.ValueBind (S.ValueBindV ["spec", "foo"] $ S.TypedTerm (S.Variable ["spec", "bar"]) $ FunctionType $ FunctionTypeN [TypeVariable ["kmkm", "prim", "int"]] $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldBe`
              [C.Definition (C.ExpressionDefinition ([], C.Int) [] "spec_foo" [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] (C.ExpressionInitializer $ C.Variable "spec_bar"))]

      describe "0" $ do
        it "foo :: () → int ⇒ int foo()" $ do
          definition def "spec" (S.ValueBind (S.ValueBindN ["spec", "foo"] [] $ S.TypedTerm (S.Literal $ S.Integer 10 10) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])]] [C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])]

        it "foo :: () → (int → int) ⇒ int (*foo())(int)" $ do
          definition def "spec" (S.ValueBind (S.ValueBindN ["spec", "foo"] [] $ S.TypedTerm (S.Variable ["spec", "bar"]) $ FunctionType $ FunctionTypeN [TypeVariable ["kmkm", "prim", "int"]] $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])], C.Pointer [], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] [C.BlockStatement $ C.Return $ C.Variable "spec_bar"])]

      describe "1" $ do
        it "foo :: int → int ⇒ int foo(int a)" $ do
          definition def "spec" (S.ValueBind (S.ValueBindN ["spec", "foo"] [("a", TypeVariable ["kmkm", "prim", "int"])] $ S.TypedTerm (S.Literal $ S.Integer 10 10) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Int), [C.Constant], Just "a", [])]] [C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])]

        it "foo :: (int → int) → int ⇒ int foo(int (*a)(int))" $ do
          definition def "spec" (S.ValueBind (S.ValueBindN ["spec", "foo"] [("a", FunctionType $ FunctionTypeN [TypeVariable ["kmkm", "prim", "int"]] $ TypeVariable ["kmkm", "prim", "int"])] $ S.TypedTerm (S.Variable ["spec", "bar"]) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Int), [], Just "a", [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]])]] [C.BlockStatement $ C.Return $ C.Variable "spec_bar"])]
