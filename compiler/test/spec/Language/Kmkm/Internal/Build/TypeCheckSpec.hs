{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Language.Kmkm.Internal.Build.TypeCheckSpec where

import Language.Kmkm.Internal.Build.TypeCheck
import Language.Kmkm.Internal.Syntax

import Instance ()

import Data.Functor.Const    (Const)
import Data.Functor.Identity
import Data.Map              (Map)
import Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          let
            actual = typeCheck mempty (Identity $ Module "spec" [["kmkm", "prim"]] [Identity $ ValueBind $ Identity $ ValueBindU ["spec", "ten"] $ Identity $ UntypedValue $ Identity $ Literal $ Identity $ Integer 10 10])
            expected :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) Identity)
            expected = Identity (Module "spec" [["kmkm", "prim"]] [Identity $ ValueBind $ Identity $ ValueBindU ["spec", "ten"] $ Identity $ TypedValue (Identity $ Literal $ Identity $ Integer 10 10) (Identity $ TypeVariable ["kmkm", "prim", "int"])])
          actual `shouldReturn` expected

        it "0x10" $ do
          let
            actual = typeCheck mempty (Identity $ Module "spec" [["kmkm", "prim"]] [Identity $ ValueBind $ Identity $ ValueBindU ["spec", "sixteen"] (Identity $ UntypedValue $ Identity $ Literal $ Identity $ Integer 16 16)])
            expected :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) Identity)
            expected = Identity (Module "spec" [["kmkm", "prim"]] [Identity $ ValueBind $ Identity $ ValueBindU ["spec", "sixteen"] $ Identity $ TypedValue (Identity $ Literal $ Identity $ Integer 16 16) (Identity $ TypeVariable ["kmkm", "prim", "int"])])
          actual `shouldReturn` expected

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              Identity $ Module
                "spec"
                []
                [ Identity $ ValueBind $
                    Identity $ ValueBindU
                      ["spec", "succ"]
                      $ Identity $ UntypedValue $
                          Identity $ TypeAnnotation $
                            Identity $ TypeAnnotation'
                              ( Identity $ UntypedValue $
                                  Identity $ Function $
                                    Identity $ FunctionC
                                      "a"
                                      (Identity $ TypeVariable ["kmkm", "prim", "int"])
                                      $ Identity $ UntypedValue $
                                          Identity $ Application $
                                            Identity $ ApplicationC
                                              (Identity $ UntypedValue $ Identity $ Variable ["spec", "succ"])
                                              (Identity $ UntypedValue $ Identity $ Variable "a")
                              )
                              $ Identity $ FunctionType $ Identity $ FunctionTypeC (Identity $ TypeVariable ["kmkm", "prim", "int"]) $ Identity $ TypeVariable ["kmkm", "prim", "int"]
                ]
            result :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) Identity)
            result =
              Identity $ Module
                "spec"
                []
                [ Identity $ ValueBind $
                    Identity $ ValueBindU ["spec", "succ"] $
                      Identity $ TypedValue
                        ( Identity $ Function $
                            Identity $ FunctionC
                              "a"
                              (Identity $ TypeVariable ["kmkm", "prim", "int"])
                              $ Identity $ TypedValue
                                  (Identity $ Application $
                                    Identity $ ApplicationC
                                      (Identity $ TypedValue (Identity $ Variable ["spec", "succ"]) (Identity $ FunctionType $ Identity $ FunctionTypeC (Identity $ TypeVariable ["kmkm", "prim", "int"]) (Identity $ TypeVariable ["kmkm", "prim", "int"])))
                                      (Identity $ TypedValue (Identity $ Variable "a") (Identity $ TypeVariable ["kmkm", "prim", "int"]))
                                  )
                                  $ Identity $ TypeVariable ["kmkm", "prim", "int"]
                        )
                        $ Identity $ FunctionType $ Identity $ FunctionTypeC (Identity $ TypeVariable ["kmkm", "prim", "int"]) $ Identity $ TypeVariable ["kmkm", "prim", "int"]
                ]
          typeCheck mempty (source) `shouldReturn` result

        it "fail" $ do
          let
            source :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Untyped (Const ()) (Const ()) Identity)
            source =
              Identity $ Module
                "spec"
                []
                [ Identity $ ValueBind $
                    Identity $ ValueBindU
                      ["spec", "succ"]
                      $ Identity $ UntypedValue $
                          Identity $ TypeAnnotation $
                            Identity $ TypeAnnotation'
                              ( Identity $ UntypedValue $
                                  Identity $ Function $
                                    Identity $ FunctionC
                                      "a"
                                      (Identity $ TypeVariable ["kmkm", "prim", "int"])
                                      (Identity $ UntypedValue $ Identity $ Variable ["spec", "succ"])
                              )
                              $ Identity $ FunctionType $ Identity $ FunctionTypeC (Identity $ TypeVariable ["kmkm", "prim", "int"]) $ Identity $ TypeVariable ["kmkm", "prim", "int"]
                ]
          typeCheck mempty source `shouldThrow` \MismatchException {} -> True

    describe "parametric polymorphism" $ do
      it "id \"hello\" is string" $ do
        let
          source :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Untyped (Const ()) (Const ()) Identity)
          source =
            Identity $ Module
              "spec"
              [["kmkm", "prim"]]
              [ Identity $ ValueBind $
                  Identity $ ValueBindU
                    ["spec", "hello"]
                    $ Identity $ UntypedValue $
                        Identity $ Application $
                          Identity $ ApplicationC
                            (Identity $ UntypedValue $ Identity $ Variable ["spec", "id"])
                            (Identity $ UntypedValue $ Identity $ Literal $ Identity $ String "hello")
              ]
          variableTypes :: Map QualifiedIdentifier (Identity (Type 'NameResolved 'Curried Identity))
          variableTypes = [(["spec", "id"], Identity $ FunctionType $ Identity $ FunctionTypeC (Identity $ TypeVariable ["a"]) $ Identity $ TypeVariable ["a"])]
          result :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const ()) (Const ()) Identity)
          result =
            Identity $ Module
              "spec"
              [["kmkm", "prim"]]
              [ Identity $ ValueBind $
                  Identity $ ValueBindU
                    ["spec", "hello"]
                    $ Identity $ TypedValue
                        ( Identity $ Application $
                            Identity $ ApplicationC
                              (Identity $ TypedValue (Identity $ Variable ["spec", "id"]) (Identity $ FunctionType $ Identity $ FunctionTypeC (Identity $ TypeVariable ["a"]) $ Identity $ TypeVariable ["a"]))
                              (Identity $ TypedValue (Identity $ Literal $ Identity $ String "hello") (Identity $ TypeVariable ["kmkm", "prim", "string"]))
                        )
                        $ Identity $ TypeVariable ["kmkm", "prim", "string"]
              ]
        typeCheck variableTypes (source) `shouldReturn` result
