{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.Pass1Spec where

import           Language.Kmkm.Builder.Pass1
import           Language.Kmkm.Syntax
import qualified Language.Kmkm.Syntax.Type   as T
import           Language.Kmkm.Syntax.Value

import Language.Kmkm.Syntax.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          typeCheck mempty (Module "spec" [] [ValueBind (ValueBindU "ten" (UntypedTerm $ Literal $ Integer 10 10)) []])
            `shouldReturn`
              Module "spec" [] [ValueBind (ValueBindU "ten" $ TypedTerm (Literal $ Integer 10 10) (T.Variable "int")) []]

        it "0x10" $ do
          typeCheck mempty (Module "spec" [] [ValueBind (ValueBindU "sixteen" (UntypedTerm $ Literal $ Integer 16 16)) []])
            `shouldReturn`
              Module "spec" [] [ValueBind (ValueBindU "sixteen" $ TypedTerm (Literal $ Integer 16 16) (T.Variable "int")) []]

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              Module
                "spec"
                []
                [ ValueBind
                    ( ValueBindU
                        "succ"
                        $ UntypedTerm $
                            TypeAnnotation $
                              TypeAnnotation'
                                ( UntypedTerm $
                                    Literal $
                                        Function $
                                          FunctionC
                                            "a"
                                            (T.Variable "int")
                                            $ UntypedTerm $
                                                Application $
                                                  ApplicationC
                                                    (UntypedTerm $ Variable $ QualifiedIdentifier (Just "spec") "succ")
                                                    (UntypedTerm $ Variable $ QualifiedIdentifier Nothing "a")
                                )
                                $ T.Function $ T.FunctionC (T.Variable "int") $ T.Variable "int"
                    )
                    []
                ]
            result =
              Module
                "spec"
                []
                [ ValueBind
                    ( ValueBindU "succ" $
                        TypedTerm
                          ( Literal $
                              Function $
                                FunctionC
                                  "a"
                                  (T.Variable "int")
                                  $ TypedTerm
                                      (Application $
                                        ApplicationC
                                          (TypedTerm (Variable $ QualifiedIdentifier (Just "spec") "succ") (T.Function $ T.FunctionC (T.Variable "int") (T.Variable "int")))
                                          (TypedTerm (Variable $ QualifiedIdentifier Nothing "a") (T.Variable "int"))
                                      )
                                      $ T.Variable "int"
                          )
                          $ T.Function $ T.FunctionC (T.Variable "int") $ T.Variable "int"
                    )
                    []
                ]
          typeCheck mempty source `shouldReturn` result

        it "fail" $ do
          let
            source =
              Module
                "spec"
                []
                [ ValueBind
                    ( ValueBindU
                        "succ"
                        $ UntypedTerm $
                            TypeAnnotation $
                              TypeAnnotation'
                                ( UntypedTerm $
                                    Literal $
                                      Function $
                                        FunctionC
                                          "a"
                                          (T.Variable "int")
                                          (UntypedTerm $ Variable $ QualifiedIdentifier (Just "spec") "succ")
                                )
                                $ T.Function $ T.FunctionC (T.Variable "int") $ T.Variable "int"
                    )
                    []
                ]
          typeCheck mempty source `shouldThrow` \MismatchException {} -> True
