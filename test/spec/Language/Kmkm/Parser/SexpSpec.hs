{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Parser.SexpSpec where

import           Language.Kmkm.Parser.Sexp
import           Language.Kmkm.Syntax
import qualified Language.Kmkm.Syntax.Type  as T
import           Language.Kmkm.Syntax.Value

import           Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  describe "parse" $ do
    describe "integer" $ do
      it "123" $ do
        parse' (integer <* M.eof) "spec" "123" `shouldReturn` Integer 123 10

      it "0o123" $ do
        parse' (integer <* M.eof) "spec" "0o123" `shouldReturn` Integer 0o123 8

      it "0x123" $ do
        parse' (integer <* M.eof) "spec" "0x123" `shouldReturn` Integer 0x123 16

      it "0b101" $ do
        parse' (integer <* M.eof) "spec" "0b101" `shouldReturn` Integer 5 2

    describe "fraction" $ do
      it "3.14" $ do
        parse' (fraction <* M.eof) "spec" "3.14" `shouldReturn` Fraction 314 2 0 10

      it "314e-2" $ do
        parse' (fraction <* M.eof) "spec" "314e-2" `shouldReturn` Fraction 314 0 (-2) 10

      it "31.4e-1" $ do
        parse' (fraction <* M.eof) "spec" "31.4e-1" `shouldReturn` Fraction 314 1 (-1) 10

      it "1e1" $ do
        parse' (fraction <* M.eof) "spec" "1e1" `shouldReturn` Fraction 1 0 1 10

      it "1e-1" $ do
        parse' (fraction <* M.eof) "spec" "1e-1" `shouldReturn` Fraction 1 0 (-1) 10

      it "0x1p1" $ do
        parse' (fraction <* M.eof) "spec" "0x1p1" `shouldReturn` Fraction 1 0 1 16

      it "0x1p-1" $ do
        parse' (fraction <* M.eof) "spec" "0x1p-1" `shouldReturn` Fraction 1 0 (-1) 16

      it "0x1.1p1" $ do
        parse' (fraction <* M.eof) "spec" "0x1.1p1" `shouldReturn` Fraction 17 1 1 16

    describe "string" $ do
      it "\"hello\"" $ do
        parse' (string <* M.eof) "spec" "\"hello\"" `shouldReturn` "hello"

      it "\"\\\"\"" $ do
        parse' (string <* M.eof) "spec" "\"\\\"\"" `shouldReturn` "\""

    describe "bind" $ do
      it "(bind foo 123 (list))" $ do
        parse' (bind <* M.eof) "spec" "(bind foo 123 (list))"
          `shouldReturn`
            ValueBind (ValueBindU "foo" (UntypedTerm $ Literal $ Integer 123 10)) []

    describe "definition" $ do
      it "(define bool (false true))" $ do
        parse' (definition <* M.eof) "spec" "(define bool (list false true))"
          `shouldReturn`
            Definition "bool" [("false", []), ("true", [])]

      it "(define book (list book (list (title string) (author string))))" $ do
        parse' (definition <* M.eof) "spec" "(define book (list (book (list (title string) (author string)))))"
          `shouldReturn`
            Definition "book" [("book", [("title", T.Variable "string"), ("author", T.Variable "string")])]

    describe "module" $ do
      it "(module math (list (bind foo 123 (list))" $ do
        parse' (module' <* M.eof) "spec" "(module math (list (bind foo 123 (list))))"
          `shouldReturn`
            Module "math" [Bind $ ValueBind (ValueBindU "foo" (UntypedTerm $ Literal $ Integer 123 10)) []]

      it "(module math (list (define bool (false true)))" $ do
        parse' (module' <* M.eof) "spec" "(module math (list (define bool (list false true))))"
          `shouldReturn`
            Module "math" [Definition "bool" [("false", []), ("true", [])]]
