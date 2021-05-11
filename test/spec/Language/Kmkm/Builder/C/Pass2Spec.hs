{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass2Spec where

import           Language.Kmkm.Builder.C.Pass2
import qualified Language.Kmkm.Builder.C.Syntax as C
import qualified Language.Kmkm.Syntax           as S
import qualified Language.Kmkm.Syntax.Type      as T
import qualified Language.Kmkm.Syntax.Value     as V

import Test.Hspec

spec :: Spec
spec = do
  describe "intermediate" $ do
    describe "bind" $ do
      describe "v" $ do
        it "foo :: int ⇒ int foo" $ do
          bind (S.TermBind (S.TermBindV "foo" $ V.TypedTerm (V.Literal $ V.Integer 10 10) $ T.Variable "int") [])
            `shouldBe`
              C.Definition (C.ExpressionDefinition ([], C.Int) [] "foo" [] (C.Expression $ C.Literal $ C.Integer 10 C.IntDecimal))

        it "foo :: int → int ⇒ int (*foo)(int)" $ do
          bind (S.TermBind (S.TermBindV "foo" $ V.TypedTerm (V.Variable "bar") $ T.Arrow $ T.Arrow1 (T.Variable "int") $ T.Variable "int") [])
            `shouldBe`
              C.Definition (C.ExpressionDefinition ([], C.Int) [] "foo" [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] (C.Expression $ C.Variable "bar"))

      describe "0" $ do
        it "foo :: () → int ⇒ int foo()" $ do
          bind (S.TermBind (S.TermBind0 "foo" $ V.TypedTerm (V.Literal $ V.Integer 10 10) $ T.Variable "int") [])
            `shouldBe`
              C.Definition (C.StatementDefinition ([], C.Int) [] "foo" [C.Function []] [C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])

        it "foo :: () → (int → int) ⇒ int (*foo())(int)" $ do
          bind (S.TermBind (S.TermBind0 "foo" $ V.TypedTerm (V.Variable "bar") $ T.Arrow $ T.Arrow1 (T.Variable "int") $ T.Variable "int") [])
            `shouldBe`
              C.Definition (C.StatementDefinition ([], C.Int) [] "foo" [C.Function [], C.Pointer [], C.Function [(([], C.Int), [C.Constant], Nothing, [])]] [C.Return $ C.Variable "bar"])

      describe "1" $ do
        it "foo :: int → int ⇒ int foo(int a)" $ do
          bind (S.TermBind (S.TermBind1 "foo" "a" (T.Variable "int") $ V.TypedTerm (V.Literal $ V.Integer 10 10) $ T.Variable "int") [])
            `shouldBe`
              C.Definition (C.StatementDefinition ([], C.Int) [] "foo" [C.Function [(([], C.Int), [C.Constant], Just "a", [])]] [C.Return $ C.Literal $ C.Integer 10 C.IntDecimal])

        it "foo :: (int → int) → int ⇒ int foo(int (*a)(int))" $ do
          bind (S.TermBind (S.TermBind1 "foo" "a" (T.Arrow $ T.Arrow1 (T.Variable "int") $ T.Variable "int") $ V.TypedTerm (V.Variable "bar") $ T.Variable "int") [])
            `shouldBe`
              C.Definition (C.StatementDefinition ([], C.Int) [] "foo" [C.Function [(([], C.Int), [], Just "a", [C.Pointer [C.Constant], C.Function [(([], C.Int), [C.Constant], Nothing, [])]])]] [C.Return $ C.Variable "bar"])
