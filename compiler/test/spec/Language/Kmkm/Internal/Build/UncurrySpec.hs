{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Language.Kmkm.Internal.Build.UncurrySpec where

import Language.Kmkm.Internal.Build.Uncurry
import Language.Kmkm.Internal.Syntax

import Utility

import Data.Functor.Const (Const)
import Prelude            hiding (uncurry)
import Test.Hspec

spec :: Spec
spec = do
  describe "uncurry" $ do
    it "for-all value" $ do
      let
        source :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
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
        result :: I (Module 'NameResolved 'Uncurried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
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
      uncurry source `shouldBe` result
