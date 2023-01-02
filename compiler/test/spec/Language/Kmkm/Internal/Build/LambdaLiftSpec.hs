{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Kmkm.Internal.Build.LambdaLiftSpec where

import           Language.Kmkm.Internal.Build.LambdaLift
import qualified Language.Kmkm.Internal.Syntax.Core.Common                                      as SC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaLifted   as S5
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaUnlifted as S4

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
      eval dataRepresentation (FI $ S4.ForAllData [["a"]] [FI $ S4.ValueConstructor "solo" [FI $ S4.Field "item" "a"]] :: DataRepresentation_)
        `shouldBe`
          FI (S5.ForAllData [["a"]] [FI $ S5.ValueConstructor "solo" [FI $ S5.Field "item" "a"]])

  describe "value" $ do
    it "(for-all a a a)" $ do
      eval value (FI $ S4.TypedValue (FI $ S4.ForAllValue "a" $ FI $ S4.TypedValue "a" "a") $ FI $ S4.ForAllType "a" "a" :: Value_)
        `shouldBe`
          (FI $ S5.TypedValue (FI $ S5.ForAllValue "a" $ FI $ S5.TypedValue "a" "a") $ FI $ S5.ForAllType "a" "a", [])

    it "1" $ do
      eval value (FI $ S4.TypedValue 1 $ FI $ S4.TypeVariable ["kmkm", "prim", "int"] :: Value_)
        `shouldBe`
          (FI $ S5.TypedValue (FI $ S5.Variable $ FI $ SC.LocalIdentifier $ SC.SystemIdentifier 'l' 0) $ FI $ S5.TypeVariable ["kmkm", "prim", "int"], [FI $ S5.ValueBindV (FI $ SC.LocalIdentifier $ SC.SystemIdentifier 'l' 0) [] 1])

  describe "lambdaLift" $ do
    it "for-all value" $ do
      let
        source :: FI (S4.Module (Const ()) (Const ()) I)
        source =
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
        result :: FI (S5.Module (Const ()) (Const ()) I)
        result =
          FI $ S5.Module
            "spec"
            []
            [ FI $ S5.ValueBindV
                ["spec", "id"]
                ["a"]
                $ FI $ S5.TypedValue "a" "a"
            ]
      lambdaLift source `shouldBe` result

  describe "peelForAll4" $ do
    it "(for-all a a)" $ do
      let
        source :: FI (S4.Value (Const ()) (Const ()) I)
        source =
          FI $ S4.TypedValue
            ( FI $ S4.ForAllValue
                "a"
                $ FI $ S4.TypedValue "a" "a"
            )
            $ FI $ S4.ForAllType "a" $ FI $ S4.TypeVariable "a"
        result = FI $ S4.TypedValue "a" "a"
      peelForAll4 source `shouldBe` (["a"], result)

eval :: (a -> Pass b) -> a -> b
eval pass = flip evalState 0 . pass

type DataRepresentation_ = FI (S4.DataRepresentation Et Ev Identity)

type Value_ = FI (S4.Value Et Ev Identity)

type Et :: a -> K.Type
data Et a = Et deriving (Show, Eq)

type Ev :: a -> K.Type
data Ev a = Ev deriving (Show, Eq)
