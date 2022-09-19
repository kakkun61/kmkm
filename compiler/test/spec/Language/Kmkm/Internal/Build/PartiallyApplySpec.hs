{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Language.Kmkm.Internal.Build.PartiallyApplySpec where

import Language.Kmkm.Internal.Build.PartiallyApply
import Language.Kmkm.Internal.Syntax

import Utility

import Data.Functor.Const (Const)
import Test.Hspec

spec :: Spec
spec = do
  describe "partiallyApply" $ do
    it "for-all value" $ do
      let
        source, result :: I (Module 'NameResolved 'Uncurried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
        source =
          I $ Module
            "spec"
            []
            [ I $ ValueBind $
                I $ ValueBindU
                  ["spec", "id"]
                  $ I $ TypedValue
                    ( I $ ForAllValue
                        "a"
                        $ I $ TypedValue
                          (I $ Variable "a")
                          $ I $ TypeVariable "a"
                    )
                    $ I $ ForAllType "a" $ I $ TypeVariable "a"
            ]
        result =
          I $ Module
            "spec"
            []
            [ I $ ValueBind $
                I $ ValueBindU
                  ["spec", "id"]
                  $ I $ TypedValue
                    ( I $ ForAllValue
                        "a"
                        $ I $ TypedValue
                          (I $ Variable "a")
                          $ I $ TypeVariable "a"
                    )
                    $ I $ ForAllType "a" $ I $ TypeVariable "a"
            ]
      partiallyApply source `shouldBe` result
