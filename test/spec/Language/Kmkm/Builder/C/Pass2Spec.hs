{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass2Spec where

import           Language.Kmkm.Builder.C.Pass2
import qualified Language.Kmkm.Builder.C.Syntax as C
import qualified Language.Kmkm.Syntax           as S
import qualified Language.Kmkm.Syntax.Type      as T
import qualified Language.Kmkm.Syntax.Value     as V

import Data.Default.Class
import Language.Kmkm.Syntax.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "intermediate" $ do
    describe "member" $ do
      describe "v" $ do
        it "foo :: int ⇒ int foo" $ do
          member def "spec" (S.ValueBind (S.ValueBindV "foo" $ V.TypedTerm (V.Literal $ V.Integer 10 10) $ T.Variable "int") [])
            `shouldBe`
              [C.Definition (C.ExpressionDefinition ([], C.Int) [] "spec_foo" [] (C.Expression $ C.Literal $ C.Integer 10 C.IntDecimal))]

        it "foo :: int → int ⇒ int (*foo)(int)" $ do
          member def "spec" (S.ValueBind (S.ValueBindV "foo" $ V.TypedTerm (V.Variable $ QualifiedIdentifier (Just "spec") "bar") $ T.Function $ T.FunctionN [T.Variable "int"] $ T.Variable "int") [])
            `shouldBe`
              [C.Definition (C.ExpressionDefinition ([], C.Int) [] "spec_foo" [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] (C.Expression $ C.Variable "spec_bar"))]

      describe "0" $ do
        it "foo :: () → int ⇒ int foo()" $ do
          member def "spec" (S.ValueBind (S.ValueBindN "foo" [] $ V.TypedTerm (V.Literal $ V.Integer 10 10) $ T.Variable "int") [])
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])]] [C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])]

        it "foo :: () → (int → int) ⇒ int (*foo())(int)" $ do
          member def "spec" (S.ValueBind (S.ValueBindN "foo" [] $ V.TypedTerm (V.Variable $ QualifiedIdentifier (Just "spec") "bar") $ T.Function $ T.FunctionN [T.Variable "int"] $ T.Variable "int") [])
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])], C.Pointer [], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] [C.BlockStatement $ C.Return $ C.Variable "spec_bar"])]

      describe "1" $ do
        it "foo :: int → int ⇒ int foo(int a)" $ do
          member def "spec" (S.ValueBind (S.ValueBindN "foo" [("a", T.Variable "int")] $ V.TypedTerm (V.Literal $ V.Integer 10 10) $ T.Variable "int") [])
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Int), [C.Constant], Just "a", [])]] [C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])]

        it "foo :: (int → int) → int ⇒ int foo(int (*a)(int))" $ do
          member def "spec" (S.ValueBind (S.ValueBindN "foo" [("a", T.Function $ T.FunctionN [T.Variable "int"] $ T.Variable "int")] $ V.TypedTerm (V.Variable $ QualifiedIdentifier (Just "spec") "bar") $ T.Variable "int") [])
            `shouldBe`
              [C.Definition (C.StatementDefinition ([], C.Int) [] "spec_foo" [C.Function [(([], C.Int), [], Just "a", [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]])]] [C.BlockStatement $ C.Return $ C.Variable "spec_bar"])]
