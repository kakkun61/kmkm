{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Kmkm.Internal.Build.LambdaLiftSpec where

import Language.Kmkm.Internal.Build.LambdaLift
import Language.Kmkm.Internal.Syntax           hiding (value)

import Utility

import           Control.Monad.State.Strict (evalState)
import           Data.Functor.Const         (Const)
import           Data.Functor.Identity      (Identity)
import qualified Data.Kind                  as K
import           Test.Hspec

spec :: Spec
spec = do
  describe "dataRepresentation" $ do
    it "(define solo (for-all a (list (solo (list (item a))))))" $ do
      eval dataRepresentation (I $ ForAllDataU [["a"]] [I $ ValueConstructor "solo" [I $ Field "item" "a"]])
        `shouldBe`
          I (ForAllDataU [["a"]] [I $ ValueConstructor "solo" [I $ Field "item" "a"]])

  describe "value" $ do
    it "(for-all a a a)" $ do
      eval value (I $ TypedValue (I $ ForAllValue "a" $ I $ TypedValue "a" "a") $ I $ ForAllType "a" "a" :: Value_)
        `shouldBe`
          (I $ TypedValue (I $ ForAllValue "a" $ I $ TypedValue "a" "a") $ I $ ForAllType "a" "a", [])

  describe "lambdaLift" $ do
    it "for-all value" $ do
      let
        source :: I (Module 'NameResolved 'Uncurried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
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
        result :: I (Module 'NameResolved 'Uncurried 'LambdaLifted 'Typed (Const ()) (Const ()) I)
        result =
          I $ Module
            "spec"
            []
            [ I $ ValueBind $
                I $ ValueBindV
                  ["spec", "id"]
                  ["a"]
                  $ I $ TypedValue
                    (I $ Variable "a")
                    $ I $ TypeVariable "a"
            ]
      lambdaLift source `shouldBe` result

  describe "peelForAll" $ do
    it "(for-all a a)" $ do
      let
        source :: I (Value 'NameResolved 'Uncurried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
        source =
          I $ TypedValue
            ( I $ ForAllValue
                "a"
                $ I $ TypedValue
                  (I $ Variable "a")
                  $ I $ TypeVariable "a"
            )
            $ I $ ForAllType "a" $ I $ TypeVariable "a"
        result =
          I $ TypedValue
            (I $ Variable "a")
            $ I $ TypeVariable "a"
      peelForAll source `shouldBe` (["a"], result)

eval :: (a -> Pass b) -> a -> b
eval pass = flip evalState 0 . pass

type Value_ = Identity (Value 'NameResolved 'Uncurried 'LambdaUnlifted 'Typed Et Ev Identity)

type Et :: a -> K.Type
data Et a = Et deriving (Show, Eq)

type Ev :: a -> K.Type
data Ev a = Ev deriving (Show, Eq)
