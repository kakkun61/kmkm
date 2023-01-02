{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.TypeCheckSpec where

import           Language.Kmkm.Internal.Build.TypeCheck
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Curried.LambdaUnlifted   as S3
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Untyped.Curried.LambdaUnlifted as S2

import Utility

import Data.Functor.Const (Const)
import Data.Map           (Map)
import Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          let
            actual = typeCheck mempty (FI $ S2.Module "spec" [["kmkm", "prim"]] [FI $ S2.ValueBind ["spec", "ten"] $ FI $ S2.Literal $ FI $ S2.Integer 10 10])
            expected :: FI (S3.Module (Const ()) (Const ()) I)
            expected = FI (S3.Module "spec" [["kmkm", "prim"]] [FI $ S3.ValueBind ["spec", "ten"] $ FI $ S3.TypedValue (FI $ S3.Literal $ FI $ S3.Integer 10 10) ["kmkm", "prim", "int"]])
          actual `shouldReturn` expected

        it "0x10" $ do
          let
            actual = typeCheck mempty (FI $ S2.Module "spec" [["kmkm", "prim"]] [FI $ S2.ValueBind ["spec", "sixteen"] $ FI $ S2.Literal $ FI $ S2.Integer 16 16])
            expected :: FI (S3.Module (Const ()) (Const ()) I)
            expected = FI (S3.Module "spec" [["kmkm", "prim"]] [FI $ S3.ValueBind ["spec", "sixteen"] $ FI $ S3.TypedValue (FI $ S3.Literal $ FI $ S3.Integer 16 16) ["kmkm", "prim", "int"]])
          actual `shouldReturn` expected

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              FI $ S2.Module
                "spec"
                []
                [ FI $ S2.ValueBind
                    ["spec", "succ"]
                    $ FI $ S2.TypeAnnotation
                        ( FI $ S2.Function
                            "a"
                            ["kmkm", "prim", "int"]
                            $ FI $ S2.Application
                                ["spec", "succ"]
                                "a"
                          )
                          $ FI $ S2.FunctionType ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]
                ]
            result :: FI (S3.Module (Const ()) (Const ()) I)
            result =
              FI $ S3.Module
                "spec"
                []
                [ FI $ S3.ValueBind
                    ["spec", "succ"]
                    $ FI $ S3.TypedValue
                        ( FI $ S3.Function
                              "a"
                              ["kmkm", "prim", "int"]
                              $ FI $ S3.TypedValue
                                  ( FI $ S3.Application
                                      (FI $ S3.TypedValue ["spec", "succ"] (FI $ S3.FunctionType ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]))
                                      (FI $ S3.TypedValue "a" ["kmkm", "prim", "int"])
                                  )
                                  ["kmkm", "prim", "int"]
                        )
                        $ FI $ S3.FunctionType ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]
                ]
          typeCheck mempty source `shouldReturn` result

        it "fail" $ do
          let
            source :: FI (S2.Module (Const ()) (Const ()) I)
            source =
              FI $ S2.Module
                "spec"
                []
                [ FI $ S2.ValueBind
                    ["spec", "succ"]
                    $ FI $ S2.TypeAnnotation
                        ( FI $ S2.Function
                            "a"
                            ["kmkm", "prim", "int"]
                            ["spec", "succ"]
                        )
                        $ FI $ S2.FunctionType ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]
                ]
          typeCheck mempty source `shouldThrow` \MismatchException {} -> True

    describe "parametric polymorphism" $ do
      it "id \"hello\" is string" $ do
        let
          source :: FI (S2.Module (Const ()) (Const ()) I)
          source =
            FI $ S2.Module
              "spec"
              [["kmkm", "prim"]]
              [ FI $ S2.ValueBind
                  ["spec", "hello"]
                  $ FI $ S2.Application
                      (FI $ S2.Instantiation ["spec", "id"] ["kmkm", "prim", "string"])
                      $ FI $ S2.Literal $ FI $ S2.String "hello"
              ]
          variableTypes :: Map S2.ReferenceIdentifier (FI (S2.Type I))
          variableTypes = [(["spec", "id"], FI $ S2.ForAllType "a" $ FI $ S2.FunctionType ["a"] ["a"])]
          result :: FI (S3.Module (Const ()) (Const ()) I)
          result =
            FI $ S3.Module
              "spec"
              [["kmkm", "prim"]]
              [ FI $ S3.ValueBind
                  ["spec", "hello"]
                  $ FI $ S3.TypedValue
                      ( FI $ S3.Application
                          ( FI $ S3.TypedValue
                              ( FI $ S3.Instantiation
                                  (FI $ S3.TypedValue ["spec", "id"] $ FI $ S3.ForAllType "a" $ FI $ S3.FunctionType ["a"] ["a"])
                                  ["kmkm", "prim", "string"]
                              )
                              $ FI $ S3.FunctionType ["kmkm", "prim", "string"] ["kmkm", "prim", "string"])
                          (FI $ S3.TypedValue (FI $ S3.Literal $ FI $ S3.String "hello") ["kmkm", "prim", "string"])
                      )
                      ["kmkm", "prim", "string"]
              ]
        typeCheck variableTypes source `shouldReturn` result

      it "for-all value" $ do
        let
          source :: FI (S2.Module (Const ()) (Const ()) I)
          source =
            FI $ S2.Module
              "spec"
              []
              [ FI $ S2.ValueBind
                  ["spec", "id"]
                  $ FI $ S2.ForAllValue "a" "a"
              ]
          result :: FI (S3.Module (Const ()) (Const ()) I)
          result =
            FI $ S3.Module
              "spec"
              []
              [ FI $ S3.ValueBind
                  ["spec", "id"]
                  $ FI $ S3.TypedValue
                    ( FI $ S3.ForAllValue
                        "a"
                        $ FI $ S3.TypedValue "a" "a"
                    )
                    $ FI $ S3.ForAllType "a" "a"
              ]
        typeCheck [] source `shouldReturn` result
