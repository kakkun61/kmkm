{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Parser.SexpSpec where

import           Language.Kmkm.Parser.Sexp
import           Language.Kmkm.Syntax
import           Language.Kmkm.Syntax.Base
import qualified Language.Kmkm.Syntax.Type  as T
import           Language.Kmkm.Syntax.Value

import           Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  describe "parse" $ do
    describe "integer" $ do
      it "123" $ do
        parse' (integer <* M.eof) "spec" "123" `shouldReturn` 123

      it "0o123" $ do
        parse' (integer <* M.eof) "spec" "0o123" `shouldReturn` 0o123

      it "0x123" $ do
        parse' (integer <* M.eof) "spec" "0x123" `shouldReturn` 0x123

      it "0b101" $ do
        parse' (integer <* M.eof) "spec" "0b101" `shouldReturn` 5

    describe "string" $ do
      it "\"hello\"" $ do
        parse' (string <* M.eof) "spec" "\"hello\"" `shouldReturn` "hello"

      it "\"\\\"\"" $ do
        parse' (string <* M.eof) "spec" "\"\\\"\"" `shouldReturn` "\""

    describe "bool" $ do
      it "true" $ do
        parse' (bool <* M.eof) "spec" "true" `shouldReturn` True

    describe "alias" $ do
      it "(alias foo 123 int)" $ do
        parse' (alias <* M.eof) "spec" "(alias foo 123 int)" `shouldReturn` Term (Identifier "foo") (Literal $ Fraction 123 0 0) (T.Variable $ Identifier "int")

    describe "definition" $ do
      it "(define bool (false true))" $ do
        parse' (definition <* M.eof) "spec" "(define bool (list false true))" `shouldReturn` Definition (Identifier "bool") [(Identifier "false", []), (Identifier "true", [])]

      it "(define book (list book (list (title string) (author string))))" $ do
        parse' (definition <* M.eof) "spec" "(define book (list (book (list (title string) (author string)))))"
          `shouldReturn`
            Definition (Identifier "book") [(Identifier "book", [(Identifier "title", T.Variable $ Identifier "string"), (Identifier "author", T.Variable $ Identifier "string")])]

    describe "module" $ do
      it "(module math (list (alias foo 123 int)" $ do
        parse' (module' <* M.eof) "spec" "(module math (list (alias foo 123 int)))"
          `shouldReturn`
            Module (Identifier "math") [Alias $ Term (Identifier "foo") (Literal $ Fraction 123 0 0) (T.Variable $ Identifier "int")]

      it "(module math (list (define bool (false true)))" $ do
        parse' (module' <* M.eof) "spec" "(module math (list (define bool (list false true))))"
          `shouldReturn`
            Module (Identifier "math") [Definition (Identifier "bool") [(Identifier "false", []), (Identifier "true", [])]]
