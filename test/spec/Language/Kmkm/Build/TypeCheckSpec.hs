{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Build.TypeCheckSpec where

import Language.Kmkm.Build.TypeCheck

import Language.Kmkm.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          typeCheck mempty (Module "spec" [] [ValueBind $ BindU "ten" $ UntypedTerm $ Literal $ Integer 10 10])
            `shouldReturn`
              Module "spec" [] [ValueBind $ BindU "ten" $ TypedTerm (Literal $ Integer 10 10) (TypeVariable "int")]

        it "0x10" $ do
          typeCheck mempty (Module "spec" [] [ValueBind $ BindU "sixteen" (UntypedTerm $ Literal $ Integer 16 16)])
            `shouldReturn`
              Module "spec" [] [ValueBind $ BindU "sixteen" $ TypedTerm (Literal $ Integer 16 16) (TypeVariable "int")]

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              Module
                "spec"
                []
                [ ValueBind $
                    BindU
                      "succ"
                      $ UntypedTerm $
                          TypeAnnotation $
                            TypeAnnotation'
                              ( UntypedTerm $
                                  Literal $
                                      Function $
                                        FunctionC
                                          "a"
                                          (TypeVariable "int")
                                          $ UntypedTerm $
                                              Application $
                                                ApplicationC
                                                  (UntypedTerm $ Variable $ QualifiedIdentifier (Just "spec") "succ")
                                                  (UntypedTerm $ Variable $ QualifiedIdentifier Nothing "a")
                              )
                              $ FunctionType $ FunctionTypeC (TypeVariable "int") $ TypeVariable "int"
                ]
            result =
              Module
                "spec"
                []
                [ ValueBind $
                    BindU "succ" $
                      TypedTerm
                        ( Literal $
                            Function $
                              FunctionC
                                "a"
                                (TypeVariable "int")
                                $ TypedTerm
                                    (Application $
                                      ApplicationC
                                        (TypedTerm (Variable $ QualifiedIdentifier (Just "spec") "succ") (FunctionType $ FunctionTypeC (TypeVariable "int") (TypeVariable "int")))
                                        (TypedTerm (Variable $ QualifiedIdentifier Nothing "a") (TypeVariable "int"))
                                    )
                                    $ TypeVariable "int"
                        )
                        $ FunctionType $ FunctionTypeC (TypeVariable "int") $ TypeVariable "int"
                ]
          typeCheck mempty source `shouldReturn` result

        it "fail" $ do
          let
            source =
              Module
                "spec"
                []
                [ ValueBind $
                    BindU
                      "succ"
                      $ UntypedTerm $
                          TypeAnnotation $
                            TypeAnnotation'
                              ( UntypedTerm $
                                  Literal $
                                    Function $
                                      FunctionC
                                        "a"
                                        (TypeVariable "int")
                                        (UntypedTerm $ Variable $ QualifiedIdentifier (Just "spec") "succ")
                              )
                              $ FunctionType $ FunctionTypeC (TypeVariable "int") $ TypeVariable "int"
                ]
          typeCheck mempty source `shouldThrow` \MismatchException {} -> True
