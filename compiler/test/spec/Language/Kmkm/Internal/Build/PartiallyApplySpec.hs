{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.PartiallyApplySpec where

import Language.Kmkm.Internal.Build.PartiallyApply
import Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted

import Utility

import Data.Functor.Const (Const)
import Test.Hspec

spec :: Spec
spec = do
  describe "partiallyApply" $ do
    it "for-all value" $ do
      let
        source, result :: FI (Module (Const ()) (Const ()) I)
        source =
          FI $ Module
            "spec"
            []
            [ FI $ ValueBind
                ["spec", "id"]
                $ FI $ TypedValue
                  ( FI $ ForAllValue
                      "a"
                      $ FI $ TypedValue "a" "a"
                  )
                  $ FI $ ForAllType "a" "a"
            ]
        result =
          FI $ Module
            "spec"
            []
            [ FI $ ValueBind
                ["spec", "id"]
                $ FI $ TypedValue
                  ( FI $ ForAllValue
                      "a"
                      $ FI $ TypedValue "a" "a"
                  )
                  $ FI $ ForAllType "a" "a"
            ]
      partiallyApply source `shouldBe` result
