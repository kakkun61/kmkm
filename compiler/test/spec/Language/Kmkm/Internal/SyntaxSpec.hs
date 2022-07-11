{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Kmkm.Internal.SyntaxSpec where

import Language.Kmkm.Internal.Syntax

import           Barbies.Bare.Layered
import           Data.Functor.Barbie.Layered
import           Data.Functor.Identity
import qualified Data.Kind                   as K
import           Test.Hspec

spec :: Spec
spec = do
  describe "bstrip" $ do
    describe "Value'" $ do
      it "Variable" $ do
        let
          actual :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped E E Bare Identity
          actual = bstrip $ Variable $ Identity "foo"
          expected = Variable "foo"
        actual `shouldBe` expected

      it "Literal" $ do
        let
          actual :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped E E Bare Identity
          actual = bstrip $ Literal $ Identity $ Integer 1 10
          expected = Literal $ Integer 1 10
        actual `shouldBe` expected

      it "Application" $ do
        let
          actual :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped E E Bare Identity
          actual = bstrip $ Application $ Identity $ ApplicationC (Identity $ UntypedValue $ Identity $ Variable $ Identity "foo") (Identity $ UntypedValue $ Identity $ Variable $ Identity "foo")
          expected = Application $ ApplicationC (UntypedValue $ Variable "foo") (UntypedValue $ Variable "foo")
        actual `shouldBe` expected

    describe "Type" $ do
      it "TypeVariable" $ do
        let
          actual :: Type 'NameUnresolved 'Curried Bare Identity
          actual = bstrip $ TypeVariable $ Identity "foo"
          expected = TypeVariable "foo"
        actual `shouldBe` expected

    describe "Definition" $ do
      it "ValueBind" $ do
        let
          actual :: Definition 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped E E Bare Identity
          actual = bstrip $ ValueBind $ ValueBindU (Identity "foo") $ Identity $ UntypedValue $ Identity $ Variable $ Identity "foo"
          expected = ValueBind $ ValueBindU "foo" $ UntypedValue $ Variable "foo"
        actual `shouldBe` expected

    it "Module" $ do
      let
        actual :: Module 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped E E Bare Identity
        actual = bstrip $ Module (Identity "foo") (Identity []) $ Identity [Identity $ ValueBind $ ValueBindU (Identity "foo") $ Identity $ UntypedValue $ Identity $ Variable $ Identity "foo"]
        expected = Module "foo" [] [ValueBind $ ValueBindU "foo" $ UntypedValue $ Variable "foo"]
      actual `shouldBe` expected

  describe "bmap" $ do
    describe "Value'" $ do
      it "Variable" $ do
        let
          a :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped E E Covered Identity
          a = bmap (Identity . runIdentity) $ Variable $ Identity "foo"
        a `shouldBe` a

type E = Const2 () Bare Identity

type Const2 :: K.Type -> k -> l -> m -> n -> K.Type
newtype Const2 a b c d e =
  Const2 a
  deriving (Show, Eq)

instance FunctorB (Const2 a b c Covered) where
  bmap _ (Const2 a) = Const2 a

instance BareB (Const2 a b c) where
  bstrip (Const2 a) = Const2 a
  bcover (Const2 a) = Const2 a
