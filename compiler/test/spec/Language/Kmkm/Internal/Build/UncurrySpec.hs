{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.UncurrySpec where

import           Language.Kmkm.Internal.Build.Uncurry
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Curried.LambdaUnlifted   as S3
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted as S4

import Utility

import Data.Functor.Const (Const)
import Prelude            hiding (uncurry)
import Test.Hspec

spec :: Spec
spec = do
  describe "uncurry" $ do
    it "for-all value" $ do
      let
        source :: FI (S3.Module (Const ()) (Const ()) I)
        source =
          FI $ S3.Module
            "spec"
            []
            [ FI $ S3.ValueBind
                ["spec", "id"]
                $ FI $ S3.TypedValue
                    ( FI $ S3.ForAllValue
                        "a"
                        $ FI $ S3.TypedValue "a" "a")
                        $ FI $ S3.ForAllType "a" "a"
            ]
        result :: FI (S4.Module (Const ()) (Const ()) I)
        result =
          FI $ S4.Module
            "spec"
            []
            [ FI $ S4.ValueBind
                ["spec", "id"]
                $ FI $ S4.TypedValue
                  ( FI $ S4.ForAllValue
                      "a"
                      $ FI $ S4.TypedValue "a" "a"
                  )
                  $ FI $ S4.ForAllType "a" "a"
            ]
      uncurry source `shouldBe` result
