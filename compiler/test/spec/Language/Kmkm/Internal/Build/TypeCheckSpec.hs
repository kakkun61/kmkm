{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Language.Kmkm.Internal.Build.TypeCheckSpec where

import Language.Kmkm.Internal.Build.TypeCheck
import Language.Kmkm.Internal.Syntax

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
            actual = typeCheck mempty (I $ Module "spec" [["kmkm", "prim"]] [I $ ValueBind $ I $ ValueBindU ["spec", "ten"] $ I $ UntypedValue $ I $ Literal $ I $ Integer 10 10])
            expected :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
            expected = I (Module "spec" [["kmkm", "prim"]] [I $ ValueBind $ I $ ValueBindU ["spec", "ten"] $ I $ TypedValue (I $ Literal $ I $ Integer 10 10) ["kmkm", "prim", "int"]])
          actual `shouldReturn` expected

        it "0x10" $ do
          let
            actual = typeCheck mempty (I $ Module "spec" [["kmkm", "prim"]] [I $ ValueBind $ I $ ValueBindU ["spec", "sixteen"] (I $ UntypedValue $ I $ Literal $ I $ Integer 16 16)])
            expected :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
            expected = I (Module "spec" [["kmkm", "prim"]] [I $ ValueBind $ I $ ValueBindU ["spec", "sixteen"] $ I $ TypedValue (I $ Literal $ I $ Integer 16 16) ["kmkm", "prim", "int"]])
          actual `shouldReturn` expected

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              I $ Module
                "spec"
                []
                [ I $ ValueBind $
                    I $ ValueBindU
                      ["spec", "succ"]
                      $ I $ UntypedValue $
                          I $ TypeAnnotation $
                            I $ TypeAnnotation'
                              ( I $ UntypedValue $
                                  I $ Function $
                                    I $ FunctionC
                                      "a"
                                      ["kmkm", "prim", "int"]
                                      $ I $ UntypedValue $
                                          I $ Application $
                                            I $ ApplicationC
                                              ["spec", "succ"]
                                              "a"
                              )
                              $ I $ FunctionType $ I $ FunctionTypeC ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]
                ]
            result :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
            result =
              I $ Module
                "spec"
                []
                [ I $ ValueBind $
                    I $ ValueBindU ["spec", "succ"] $
                      I $ TypedValue
                        ( I $ Function $
                            I $ FunctionC
                              "a"
                              ["kmkm", "prim", "int"]
                              $ I $ TypedValue
                                  (I $ Application $
                                    I $ ApplicationC
                                      (I $ TypedValue ["spec", "succ"] (I $ FunctionType $ I $ FunctionTypeC ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]))
                                      (I $ TypedValue "a" ["kmkm", "prim", "int"])
                                  )
                                  ["kmkm", "prim", "int"]
                        )
                        $ I $ FunctionType $ I $ FunctionTypeC ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]
                ]
          typeCheck mempty source `shouldReturn` result

        it "fail" $ do
          let
            source :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Untyped (Const ()) (Const ()) I)
            source =
              I $ Module
                "spec"
                []
                [ I $ ValueBind $
                    I $ ValueBindU
                      ["spec", "succ"]
                      $ I $ UntypedValue $
                          I $ TypeAnnotation $
                            I $ TypeAnnotation'
                              ( I $ UntypedValue $
                                  I $ Function $
                                    I $ FunctionC
                                      "a"
                                      ["kmkm", "prim", "int"]
                                      ["spec", "succ"]
                              )
                              $ I $ FunctionType $ I $ FunctionTypeC ["kmkm", "prim", "int"] ["kmkm", "prim", "int"]
                ]
          typeCheck mempty source `shouldThrow` \MismatchException {} -> True

    describe "parametric polymorphism" $ do
      it "id \"hello\" is string" $ do
        let
          source :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Untyped (Const ()) (Const ()) I)
          source =
            I $ Module
              "spec"
              [["kmkm", "prim"]]
              [ I $ ValueBind $
                  I $ ValueBindU
                    ["spec", "hello"]
                    $ I $ UntypedValue $
                        I $ Application $
                          I $ ApplicationC
                            (I $ UntypedValue $ I $ Instantiation $ I $ InstantiationC ["spec", "id"] ["kmkm", "prim", "string"])
                            (I $ UntypedValue $ I $ Literal $ I $ String "hello")
              ]
          variableTypes :: Map QualifiedIdentifier (I (Type 'NameResolved 'Curried I))
          variableTypes = [(["spec", "id"], I $ ForAllType "a" $ I $ FunctionType $ I $ FunctionTypeC ["a"] ["a"])]
          result :: I (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) I)
          result =
            I $ Module
              "spec"
              [["kmkm", "prim"]]
              [ I $ ValueBind $
                  I $ ValueBindU
                    ["spec", "hello"]
                    $ I $ TypedValue
                        ( I $ Application $
                            I $ ApplicationC
                              ( I $ TypedValue
                                  ( I $ Instantiation $ I $ InstantiationC
                                      (I $ TypedValue ["spec", "id"] $ I $ ForAllType "a" $ I $ FunctionType $ I $ FunctionTypeC ["a"] ["a"])
                                      ["kmkm", "prim", "string"]
                                  )
                                  $ I $ FunctionType $ I $ FunctionTypeC ["kmkm", "prim", "string"] ["kmkm", "prim", "string"])
                              (I $ TypedValue (I $ Literal $ I $ String "hello") ["kmkm", "prim", "string"])
                        )
                        ["kmkm", "prim", "string"]
              ]
        typeCheck variableTypes source `shouldReturn` result
