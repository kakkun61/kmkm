{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass2Spec where

import           Language.Kmkm.Builder.C.Pass2
import qualified Language.Kmkm.Builder.C.Syntax as C
import qualified Language.Kmkm.Syntax           as S

import Data.Default.Class
import Language.Kmkm.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "intermediate" $ do
    describe "definition" $ do
      describe "v" $ do
        it "foo :: int ⇒ int foo" $ do
          definition def "spec" True (S.ValueBind (S.BindV "foo" $ S.TypedTerm (S.Literal $ S.Integer 10 10) $ TypeVariable "int"))
            `shouldBe`
              [C.Definition (C.ExpressionDefinition ([], C.Int) [] "spec_foo" [] (C.ExpressionInitializer $ C.Literal $ C.Integer 10 C.IntDecimal))]

        it "foo :: int → int ⇒ int (*foo)(int)" $ do
          definition def "spec" True (S.ValueBind (S.BindV "foo" $ S.TypedTerm (S.Variable $ QualifiedIdentifier (Just "spec") "bar") $ FunctionType $ FunctionTypeN [TypeVariable "int"] $ TypeVariable "int"))
            `shouldBe`
              [C.Definition (C.ExpressionDefinition ([], C.Int) [] "spec_foo" [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] (C.ExpressionInitializer $ C.Variable "spec_bar"))]

      describe "0" $ do
        it "foo :: () → int ⇒ int foo()" $ do
          definition def "spec" True (S.ValueBind (S.BindN "foo" [] $ S.TypedTerm (S.Literal $ S.Integer 10 10) $ TypeVariable "int"))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])]] [C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])]

        it "foo :: () → (int → int) ⇒ int (*foo())(int)" $ do
          definition def "spec" True (S.ValueBind (S.BindN "foo" [] $ S.TypedTerm (S.Variable $ QualifiedIdentifier (Just "spec") "bar") $ FunctionType $ FunctionTypeN [TypeVariable "int"] $ TypeVariable "int"))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])], C.Pointer [], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] [C.BlockStatement $ C.Return $ C.Variable "spec_bar"])]

      describe "1" $ do
        it "foo :: int → int ⇒ int foo(int a)" $ do
          definition def "spec" True (S.ValueBind (S.BindN "foo" [("a", TypeVariable "int")] $ S.TypedTerm (S.Literal $ S.Integer 10 10) $ TypeVariable "int"))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Int), [C.Constant], Just "a", [])]] [C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])]

        it "foo :: (int → int) → int ⇒ int foo(int (*a)(int))" $ do
          definition def "spec" True (S.ValueBind (S.BindN "foo" [("a", FunctionType $ FunctionTypeN [TypeVariable "int"] $ TypeVariable "int")] $ S.TypedTerm (S.Variable $ QualifiedIdentifier (Just "spec") "bar") $ TypeVariable "int"))
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Int), [], Just "a", [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]])]] [C.BlockStatement $ C.Return $ C.Variable "spec_bar"])]
