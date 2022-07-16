{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Language.Kmkm.Internal.SyntaxSpec where

import Language.Kmkm.Internal.Syntax

import Data.Functor.Barbie.Layered
import Data.Functor.Const          (Const)
import Data.Functor.Identity
import Test.Hspec

spec :: Spec
spec = do
  describe "bmap" $ do
    describe "Value'" $ do
      it "Variable" $ do
        let
          a :: Value' 'NameUnresolved 'Curried 'LambdaUnlifted 'Untyped (Const ()) (Const ()) Identity
          a = bmap (Identity . runIdentity) $ Variable $ Identity "foo"
        a `shouldBe` a
