{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.SyntaxSpec where

import Language.Kmkm.Internal.Syntax

import Barbies.Bare.Layered
import Data.Functor.Barbie.Layered
import Data.Functor.Identity
import Test.Hspec

spec :: Spec
spec = do
  describe "bstrip" $ do
    describe "Value'" $ do
      it "Variable" $ do
        let
          actual = bstrip $ Variable $ Identity "foo" :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped Bare Identity
          expected = Variable "foo"
        actual `shouldBe` expected

      it "Literal" $ do
        let
          actual = bstrip $ Literal $ Integer 1 10 :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped Bare Identity
          expected = Literal $ Integer 1 10
        actual `shouldBe` expected

      it "Application" $ do
        let
          actual = bstrip $ Application $ ApplicationC (Identity $ UntypedValue $ Identity $ Variable $ Identity "foo") (Identity $ UntypedValue $ Identity $ Variable $ Identity "foo") :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped Bare Identity
          expected = Application $ ApplicationC (UntypedValue $ Variable "foo") (UntypedValue $ Variable "foo")
        actual `shouldBe` expected

    describe "Type" $ do
      it "TypeVariable" $ do
        let
          actual = bstrip $ TypeVariable $ Identity "foo" :: Type 'NameUnresolved 'Curried Bare Identity
          expected = TypeVariable "foo"
        actual `shouldBe` expected

    describe "Definition" $ do
      it "ValueBind" $ do
        let
          actual = bstrip $ ValueBind $ ValueBindU (Identity "foo") $ Identity $ UntypedValue $ Identity $ Variable $ Identity "foo" :: Definition 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped Bare Identity
          expected = ValueBind $ ValueBindU "foo" $ UntypedValue $ Variable "foo"
        actual `shouldBe` expected

    it "Module" $ do
      let
        actual = bstrip $ Module (Identity "foo") (Identity []) $ Identity [Identity $ ValueBind $ ValueBindU (Identity "foo") $ Identity $ UntypedValue $ Identity $ Variable $ Identity "foo"] :: Module 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped Bare Identity
        expected = Module "foo" [] [ValueBind $ ValueBindU "foo" $ UntypedValue $ Variable "foo"]
      actual `shouldBe` expected

  describe "bmap" $ do
    describe "Value'" $ do
      it "Variable" $ do
        let
          a = bmap (Identity . runIdentity) $ Variable $ Identity "foo" :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped Covered Identity
        a `shouldBe` a
