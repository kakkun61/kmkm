{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Parse.SexpSpec where

import Language.Kmkm.Internal.Parse.Sexp
import Language.Kmkm.Internal.Syntax

import           Barbies.Bare.Layered (bstripFrom)
import           Data.Copointed       (Copointed (copoint))
import           Test.Hspec
import qualified Text.Megaparsec      as M

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

    describe "identifier" $ do
      it "foo" $ do
        parse' (identifier <* M.eof) "spec" "foo"
          `shouldSatisfy`
            (\case
              Right i -> copoint i == "foo"
              Left _  -> False
            )

    describe "valueBind" $ do
      it "bind-value foo 123" $ do
        parse' (valueBind <* M.eof) "spec" "bind-value foo 123"
          `shouldSatisfy`
            (\case
              Right b -> bstripFrom copoint b == ValueBind (ValueBindU "foo" $ UntypedValue $ Literal $ Integer 123 10)
              Left _  -> False
            )

    describe "dataDefinition" $ do
      it "define bool (false true)" $ do
        parse' (dataDefinition <* M.eof) "spec" "define bool (list false true)"
          `shouldSatisfy`
            (\case
              Right b -> bstripFrom copoint b == DataDefinition "bool" [("false", []), ("true", [])]
              Left _  -> False
            )

      it "define book (list book (list (title string) (author string)))" $ do
        parse' (dataDefinition <* M.eof) "spec" "define book (list (book (list (title string) (author string))))"
          `shouldSatisfy`
            (\case
              Right b -> bstripFrom copoint b == DataDefinition "book" [("book", [("title", TypeVariable "string"), ("author", TypeVariable "string")])]
              Left _ -> False
            )

    describe "module" $ do
      it "(module math (list) (list (bind-value foo 123)" $ do
        parse' (module' <* M.eof) "spec" "(module math (list) (list (bind-value foo 123)))"
          `shouldSatisfy`
            (\case
              Right m -> bstripFrom copoint (copoint m) == Module "math" [] [ValueBind $ ValueBindU "foo" $ UntypedValue $ Literal $ Integer 123 10]
              Left _ -> False
            )

      it "(module math (list) (list (define bool (false true)))" $ do
        parse' (module' <* M.eof) "spec" "(module math (list) (list (define bool (list false true))))"
          `shouldSatisfy`
            (\case
              Right m -> bstripFrom copoint (copoint m) == Module "math" [] [DataDefinition "bool" [("false", []), ("true", [])]]
              Left _ -> False
            )
