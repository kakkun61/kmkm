{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Language.Kmkm.Internal.SyntaxSpec where

import Language.Kmkm.Internal.Syntax

import Utility

import Data.Functor.Barbie.Layered (FunctorB (bmap))
import Data.Functor.Const          (Const)
import Data.Functor.Identity       (Identity (runIdentity))
import Test.Hspec

spec :: Spec
spec = do
  describe "bmap" $ do
    describe "Value'" $ do
      it "Variable" $ do
        let
          a :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped (Const ()) (Const ()) I
          a = bmap (I . runIdentity) "foo"
        a `shouldBe` a
