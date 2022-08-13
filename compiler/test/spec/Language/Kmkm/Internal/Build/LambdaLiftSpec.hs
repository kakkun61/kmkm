{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.LambdaLiftSpec where

import Language.Kmkm.Internal.Build.LambdaLift
import Language.Kmkm.Internal.Syntax

import Utility

import Test.Hspec
import Control.Monad.State.Strict (evalState)

spec :: Spec
spec = do
  describe "definition" $ do
    it "(define solo (for-all a (list (solo (list (item a))))))" $ do
      eval dataRepresentation (I $ ForAllDataU [["a"]] [I $ ValueConstructor "solo" [I $ Field "item" "a"]])
        `shouldBe`
          I (ForAllDataU [["a"]] [I $ ValueConstructor "solo" [I $ Field "item" "a"]])

eval :: (a -> Pass b) -> a -> b
eval pass = flip evalState 0 . pass
