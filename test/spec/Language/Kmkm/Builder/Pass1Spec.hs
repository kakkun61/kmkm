{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.Pass1Spec where

import           Language.Kmkm.Builder.Pass1
import           Language.Kmkm.Syntax
import           Language.Kmkm.Syntax.Base
import qualified Language.Kmkm.Syntax.Type   as T
import           Language.Kmkm.Syntax.Value
import           Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          typeCheck (Module (Identifier "") [Bind $ Term (Identifier "ten") (UntypedTerm $ Literal $ Integer 10 10) (T.Variable $ Identifier "int")])
            `shouldReturn`
              Module (Identifier "") [Bind $ Term (Identifier "ten") (TypedTerm (Literal $ Integer 10 10) (T.Variable $ Identifier "int")) (T.Variable $ Identifier "int")]

        it "0x10" $ do
          typeCheck (Module (Identifier "") [Bind $ Term (Identifier "sixteen") (UntypedTerm $ Literal $ Integer 16 16) (T.Variable $ Identifier "int")])
            `shouldReturn`
              Module (Identifier "") [Bind $ Term (Identifier "sixteen") (TypedTerm (Literal $ Integer 16 16) (T.Variable $ Identifier "int")) (T.Variable $ Identifier "int")]

    describe "application" $ do
      describe "succ" $ do
        it "success" $ do
          let
            source =
              Module
                (Identifier "")
                [ Bind $
                    Term
                      (Identifier "succ")
                      ( UntypedTerm $
                          Literal $
                            Function' $
                              FunctionC
                                (Identifier "a")
                                (T.Variable $ Identifier "int")
                                ( UntypedTerm $
                                    Application' $
                                      ApplicationC
                                        (UntypedTerm $ Variable (Identifier "succ"))
                                        (UntypedTerm $ Variable (Identifier "a"))
                                )
                      )
                      (T.Arrow' $ T.ArrowC (T.Variable $ Identifier "int") (T.Variable $ Identifier "int"))
                ]
            result =
              Module
                (Identifier "")
                [ Bind $
                    Term
                      (Identifier "succ")
                      ( TypedTerm
                          ( Literal $
                              Function' $
                                FunctionC
                                  (Identifier "a")
                                  (T.Variable $ Identifier "int")
                                  ( TypedTerm
                                      (Application' $
                                        ApplicationC
                                          (TypedTerm (Variable $ Identifier "succ") (T.Arrow' $ T.ArrowC (T.Variable $ Identifier "int") (T.Variable $ Identifier "int")))
                                          (TypedTerm (Variable $ Identifier "a") (T.Variable $ Identifier "int"))
                                      )
                                      (T.Variable $ Identifier "int")
                                  )
                          )
                          (T.Arrow' $ T.ArrowC (T.Variable $ Identifier "int") (T.Variable $ Identifier "int"))
                      )
                      (T.Arrow' $ T.ArrowC (T.Variable $ Identifier "int") (T.Variable $ Identifier "int"))
                ]
          typeCheck source `shouldReturn` result

        it "fail" $ do
          let
            source =
              Module
                (Identifier "")
                [ Bind $
                    Term
                      (Identifier "succ")
                      (UntypedTerm $
                        Literal $
                          Function' $
                            FunctionC
                              (Identifier "a")
                              (T.Variable $ Identifier "int")
                              (UntypedTerm $ Variable (Identifier "succ"))
                      )
                      (T.Arrow' $ T.ArrowC (T.Variable $ Identifier "int") (T.Variable $ Identifier "int"))
                ]
          typeCheck source `shouldThrow` \MismatchException {} -> True
