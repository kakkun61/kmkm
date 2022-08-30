{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Language.Kmkm.Internal.Build.LambdaLiftSpec where

import Language.Kmkm.Internal.Build.LambdaLift
import Language.Kmkm.Internal.Syntax hiding (value)

import Utility

import Control.Monad.State.Strict (evalState)
import Test.Hspec
import Data.Functor.Identity (Identity)
import qualified Data.Kind as K

spec :: Spec
spec = do
  describe "dataRepresentation" $ do
    it "(define solo (for-all a (list (solo (list (item a))))))" $ do
      eval dataRepresentation (I $ ForAllDataU [["a"]] [I $ ValueConstructor "solo" [I $ Field "item" "a"]])
        `shouldBe`
          I (ForAllDataU [["a"]] [I $ ValueConstructor "solo" [I $ Field "item" "a"]])

  describe "value" $ do
    it "(for-all 0)" $ do
      eval value (I $ TypedValue (I $ ForAllValue "a" $ I $ TypedValue "a" "a") "a" :: Value_)
        `shouldBe`
          (I $ TypedValue (I $ ForAllValue "a" $ I $ TypedValue "a" "a") "a", [])

eval :: (a -> Pass b) -> a -> b
eval pass = flip evalState 0 . pass

type Value_ = Identity (Value 'NameResolved 'Uncurried 'LambdaUnlifted 'Typed Et Ev Identity)

type Et :: a -> K.Type
data Et a = Et deriving (Show, Eq)

type Ev :: a -> K.Type
data Ev a = Ev deriving (Show, Eq)
