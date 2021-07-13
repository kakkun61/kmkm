{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Build.TypeCheckSpec where

import Language.Kmkm.Build.TypeCheck

import Barbies.Bare.Layered
import Data.Functor.Identity
import Language.Kmkm.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          typeCheck mempty (cover $ Module "spec" [] [ValueBind $ ValueBindU ["spec", "ten"] $ UntypedValue $ Literal $ Integer 10 10])
            `shouldReturn`
              cover (Module "spec" [] [ValueBind $ ValueBindU ["spec", "ten"] $ TypedValue (Literal $ Integer 10 10) (TypeVariable ["kmkm", "prim", "int"])])

        it "0x10" $ do
          typeCheck mempty (cover $ Module "spec" [] [ValueBind $ ValueBindU ["spec", "sixteen"] (UntypedValue $ Literal $ Integer 16 16)])
            `shouldReturn`
              cover (Module "spec" [] [ValueBind $ ValueBindU ["spec", "sixteen"] $ TypedValue (Literal $ Integer 16 16) (TypeVariable ["kmkm", "prim", "int"])])

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              Module
                "spec"
                []
                [ ValueBind $
                    ValueBindU
                      ["spec", "succ"]
                      $ UntypedValue $
                          TypeAnnotation $
                            TypeAnnotation'
                              ( UntypedValue $
                                  Function $
                                    FunctionC
                                      "a"
                                      (TypeVariable ["kmkm", "prim", "int"])
                                      $ UntypedValue $
                                          Application $
                                            ApplicationC
                                              (UntypedValue $ Variable ["spec", "succ"])
                                              (UntypedValue $ Variable "a")
                              )
                              $ FunctionType $ FunctionTypeC (TypeVariable ["kmkm", "prim", "int"]) $ TypeVariable ["kmkm", "prim", "int"]
                ]
            result =
              Module
                "spec"
                []
                [ ValueBind $
                    ValueBindU ["spec", "succ"] $
                      TypedValue
                        ( Function $
                            FunctionC
                              "a"
                              (TypeVariable ["kmkm", "prim", "int"])
                              $ TypedValue
                                  (Application $
                                    ApplicationC
                                      (TypedValue (Variable ["spec", "succ"]) (FunctionType $ FunctionTypeC (TypeVariable ["kmkm", "prim", "int"]) (TypeVariable ["kmkm", "prim", "int"])))
                                      (TypedValue (Variable "a") (TypeVariable ["kmkm", "prim", "int"]))
                                  )
                                  $ TypeVariable ["kmkm", "prim", "int"]
                        )
                        $ FunctionType $ FunctionTypeC (TypeVariable ["kmkm", "prim", "int"]) $ TypeVariable ["kmkm", "prim", "int"]
                ]
          typeCheck mempty (cover source) `shouldReturn` cover result

        it "fail" $ do
          let
            source =
              Module
                "spec"
                []
                [ ValueBind $
                    ValueBindU
                      ["spec", "succ"]
                      $ UntypedValue $
                          TypeAnnotation $
                            TypeAnnotation'
                              ( UntypedValue $
                                  Function $
                                    FunctionC
                                      "a"
                                      (TypeVariable ["kmkm", "prim", "int"])
                                      (UntypedValue $ Variable ["spec", "succ"])
                              )
                              $ FunctionType $ FunctionTypeC (TypeVariable ["kmkm", "prim", "int"]) $ TypeVariable ["kmkm", "prim", "int"]
                ]
          typeCheck mempty (cover source) `shouldThrow` \MismatchException {} -> True

cover :: BareB b => b Bare Identity -> Identity (b Covered Identity)
cover = Identity . bcoverWith Identity
