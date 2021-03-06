{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Kmkm.Internal.Parse.SexpSpec where

import           Language.Kmkm.Internal.Parse.Sexp
import qualified Language.Kmkm.Internal.Parse.Sexp.C as C
import           Language.Kmkm.Internal.Syntax
import           Language.Kmkm.Internal.Syntax.Sexp

import Utility

import           Control.Monad.Catch     (MonadCatch, MonadThrow)
import           Control.Monad.Except    (Except)
import           Control.Monad.Reader    (ReaderT)
import           Data.Functor.Identity   (Identity (runIdentity))
import           Data.Text               (Text)
import           Data.Void               (Void)
import           Test.Hspec
import qualified Text.Megaparsec         as M
import           Text.Megaparsec.Parsers (ParsecT (unParsecT))
import qualified Text.Parser.Combinators as P

spec :: Spec
spec = do
  describe "parsec" $ do
    describe "string" $ do
      it "\"\"" $ do
        pc stringSingleDoubleQuotationText "\"\"" `shouldReturn` ""

      it "\"foo\"" $ do
        pc stringSingleDoubleQuotationText "\"foo\"" `shouldReturn` "foo"

      it "\"\\\"\"" $ do
        pc stringSingleDoubleQuotationText "\"\\\"\"" `shouldReturn` "\""

      it "\"\"\"foo\"\"\"" $ do
        pc stringTripleDoubleQuotationText "\"\"\"foo\"\"\"" `shouldReturn` "foo"

      it "\"\"\"\"foo\"\"\"\"" $ do
        pc stringTripleDoubleQuotationText "\"\"\"\"foo\"\"\"\"" `shouldReturn` "\"foo\""

  describe "parseText" $ do
    it "parses a simple atom" $ do
      pt "spec" "foo" `shouldReturn` "foo"

    it "parses a null list" $ do
      pt "spec" "()" `shouldReturn` []

    it "parses a singleton list" $ do
      pt "spec" "(foo)" `shouldReturn` ["foo"]

    it "parses a 2-element list" $ do
      pt "spec" "(foo bar)" `shouldReturn` ["foo", "bar"]

    it "parses a string with space" $ do
      pt "spec" "(\"foo bar\")" `shouldReturn` ["\"foo bar\""]

  describe "parseSexp'" $ do
    describe "integer" $ do
      it "123" $ do
        ps integer "spec" "123" `shouldReturn` Integer 123 10

      it "0o123" $ do
        ps integer "spec" "0o123" `shouldReturn` Integer 0o123 8

      it "0x123" $ do
        ps integer "spec" "0x123" `shouldReturn` Integer 0x123 16

      it "0b101" $ do
        ps integer "spec" "0b101" `shouldReturn` Integer 5 2

    describe "fraction" $ do
      it "3.14" $ do
        ps fraction "spec" "3.14" `shouldReturn` Fraction 314 2 0 10

      it "314e-2" $ do
        ps fraction "spec" "314e-2" `shouldReturn` Fraction 314 0 (-2) 10

      it "31.4e-1" $ do
        ps fraction "spec" "31.4e-1" `shouldReturn` Fraction 314 1 (-1) 10

      it "1e1" $ do
        ps fraction "spec" "1e1" `shouldReturn` Fraction 1 0 1 10

      it "1e-1" $ do
        ps fraction "spec" "1e-1" `shouldReturn` Fraction 1 0 (-1) 10

      it "0x1p1" $ do
        ps fraction "spec" "0x1p1" `shouldReturn` Fraction 1 0 1 16

      it "0x1p-1" $ do
        ps fraction "spec" "0x1p-1" `shouldReturn` Fraction 1 0 (-1) 16

      it "0x1.1p1" $ do
        ps fraction "spec" "0x1.1p1" `shouldReturn` Fraction 17 1 1 16

    describe "string" $ do
      it "\"hello\"" $ do
        ps string "spec" "\"hello\"" `shouldReturn` "hello"

      it "\"\\\"\"" $ do
        ps string "spec" "\"\\\"\"" `shouldReturn` "\""

    describe "identifier" $ do
      it "foo" $ do
        ps identifier "spec" "foo" `shouldReturn` "foo"

    describe "valueBind" $ do
      it "bind-value foo 123" $ do
        ps valueBind "spec" ["bind-value", "foo", "123"]
          `shouldReturn`
            ( ValueBindU "foo" (I $ UntypedValue $ I $ Literal $ I $ Integer 123 10)
                :: ValueBind 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped EmbeddedCType EmbeddedCValue I
            )

    describe "foreignValueBind" $ do
      it "bind-value-foreign foo (list (c-value \"\" (list) \"\")) foo" $
        ps foreignValueBind "spec" ["bind-value-foreign", "foo", ["list", ["c-value", "\"\"", ["list"], "\"\""]], "foo"]
          `shouldReturn`
            ( ForeignValueBind (I "foo") (I $ EmbeddedCValue "" (I []) "") "foo"
                :: Definition 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped EmbeddedCType EmbeddedCValue I
            )

    describe "foreignTypeBind" $ do
      it "bind-type-foreign foo (list (c-type \"\" \"\"))" $
        ps foreignTypeBind "spec" ["bind-type-foreign", "foo", ["list", ["c-type", "\"\"", "\"\""]]]
          `shouldReturn`
            ForeignTypeBind (I "foo") (I $ EmbeddedCType "" "")

    describe "dataDefinition" $ do
      it "define bool (false true)" $ do
        ps dataDefinition "spec" ["define", "bool", ["list", "false", "true"]]
          `shouldReturn`
            ( DataDefinition "bool" (I [I (ValueConstructor "false" $ I []), I (ValueConstructor "true" $ I [])])
                :: Definition 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped EmbeddedCType EmbeddedCValue I
            )

      it "define book (list (book (list (title string) (author string))))" $ do
        ps dataDefinition "spec" ["define", "book", ["list", ["book", ["list", ["title", "string"], ["author", "string"]]]]]
          `shouldReturn`
            ( DataDefinition "book" (I [I (ValueConstructor "book" $ I [I (Field "title" "string"), I (Field "author" "string")])])
                :: Definition 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped EmbeddedCType EmbeddedCValue I
            )

    describe "definition" $ do
      it "bind-type-foreign foo (list (c-type \"\" \"unsigned int\"))" $
        ps definition "spec" ["bind-type-foreign", "foo", ["list", ["c-type", "\"\"", "\"unsigned int\""]]]
          `shouldReturn`
            ForeignTypeBind (I "foo") (I $ EmbeddedCType "" "unsigned int")

      it "bind-value-foreign foo (list (c-value \"\" (list) \"\")) foo" $
        ps definition "spec" ["bind-value-foreign", "foo", ["list", ["c-value", "\"\"", ["list"], "\"\""]], "foo"]
          `shouldReturn`
            ( ForeignValueBind (I "foo") (I $ EmbeddedCValue "" (I []) "") "foo"
                :: Definition 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped EmbeddedCType EmbeddedCValue I
            )

    describe "module" $ do
      it "(module math (list) (list (bind-value foo 123)" $ do
        ps module' "spec" ["module", "math", ["list"], ["list", ["bind-value", "foo", "123"]]]
          `shouldReturn`
            Module "math" (I []) (I [I $ ValueBind $ I $ ValueBindU "foo" $ I $ UntypedValue $ I $ Literal $ I $ Integer 123 10])

      it "(module math (list) (list (define bool (false true)))" $ do
        ps module' "spec" ["module", "math", ["list"], ["list", ["define", "bool", ["list", "false", "true"]]]]
          `shouldReturn` Module (I "math") (I []) (I [I $ DataDefinition (I "bool") (I [I (ValueConstructor "false" $ I []), I (ValueConstructor "true" $ I [])])])

pt :: MonadThrow m => String -> Text -> m (I (Sexp I))
pt label text = toIdentity <$> parseText label text

ps :: MonadCatch m => (I (Sexp I) -> ReaderT (Env EmbeddedCType EmbeddedCValue I) (Except [Exception]) (I a)) -> String -> I (Sexp I) -> m a
ps parser label sexp = runIdentity <$> parseSexp' parser [C.embeddedParser] (traverse $ \(EmbeddedTypeC t) -> Just t) (traverse $ \(EmbeddedValueC v) -> Just v) label sexp

pc :: MonadFail m => ParsecT Void Text I a -> Text -> m a
pc parser input =
  case M.runParser (unParsecT $ parser <* P.eof) "spec" input of
    Right s -> pure s
    Left e  -> fail $ M.errorBundlePretty e
