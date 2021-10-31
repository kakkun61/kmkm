{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Kmkm.Internal.Build.TypeCheckSpec where

import Language.Kmkm.Internal.Build.TypeCheck
import Language.Kmkm.Internal.Syntax

import           Barbies.Bare.Layered
import           Data.Functor.Barbie.Layered
import           Data.Functor.Identity
import qualified Data.Kind                   as K
import           Data.Map                    (Map)
import           Test.Hspec

spec :: Spec
spec = do
  describe "typeCheck" $ do
    describe "literal" $ do
      describe "integer" $ do
        it "10" $ do
          let
            actual = typeCheck mempty (cover $ Module "spec" [["kmkm", "prim"]] [ValueBind $ ValueBindU ["spec", "ten"] $ UntypedValue $ Literal $ Integer 10 10])
            expected :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const2 () Covered Identity) (Const2 () Covered Identity) Covered Identity)
            expected = cover (Module "spec" [["kmkm", "prim"]] [ValueBind $ ValueBindU ["spec", "ten"] $ TypedValue (Literal $ Integer 10 10) (TypeVariable ["kmkm", "prim", "int"])])
          actual `shouldReturn` expected

        it "0x10" $ do
          let
            actual = typeCheck mempty (cover $ Module "spec" [["kmkm", "prim"]] [ValueBind $ ValueBindU ["spec", "sixteen"] (UntypedValue $ Literal $ Integer 16 16)])
            expected :: Identity (Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const2 () Covered Identity) (Const2 () Covered Identity) Covered Identity)
            expected = cover (Module "spec" [["kmkm", "prim"]] [ValueBind $ ValueBindU ["spec", "sixteen"] $ TypedValue (Literal $ Integer 16 16) (TypeVariable ["kmkm", "prim", "int"])])
          actual `shouldReturn` expected

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
            result :: Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const2 () Covered Identity) (Const2 () Bare Identity) Bare Identity
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
            source :: Module 'NameResolved 'Curried 'LambdaUnlifted 'Untyped (Const2 () Covered Identity) (Const2 () Bare Identity) Bare Identity
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

    describe "parametric polymorphism" $ do
      it "id \"hello\" is string" $ do
        let
          source :: Module 'NameResolved 'Curried 'LambdaUnlifted 'Untyped (Const2 () Covered Identity) (Const2 () Bare Identity) Bare Identity
          source =
            Module
              "spec"
              [["kmkm", "prim"]]
              [ ValueBind $
                  ValueBindU
                    ["spec", "hello"]
                    $ UntypedValue $
                        Application $
                          ApplicationC
                            (UntypedValue $ Variable ["spec", "id"])
                            (UntypedValue $ Literal $ String "hello")
              ]
          variableTypes :: Map QualifiedIdentifier (Identity (Type 'NameResolved 'Curried Covered Identity))
          variableTypes = [(["spec", "id"], cover $ FunctionType $ FunctionTypeC (TypeVariable ["a"]) $ TypeVariable ["a"])]
          result :: Module 'NameResolved 'Curried 'LambdaUnlifted 'Typed (Const2 () Covered Identity) (Const2 () Bare Identity) Bare Identity
          result =
            Module
              "spec"
              [["kmkm", "prim"]]
              [ ValueBind $
                  ValueBindU
                    ["spec", "hello"]
                    $ TypedValue
                        ( Application $
                            ApplicationC
                              (TypedValue (Variable ["spec", "id"]) (FunctionType $ FunctionTypeC (TypeVariable ["a"]) $ TypeVariable ["a"]))
                              (TypedValue (Literal $ String "hello") (TypeVariable ["kmkm", "prim", "string"]))
                        )
                        $ TypeVariable ["kmkm", "prim", "string"]
              ]
        typeCheck variableTypes (cover source) `shouldReturn` cover result

cover :: BareB b => b Bare Identity -> Identity (b Covered Identity)
cover = Identity . bcoverWith Identity

type Const2 :: K.Type -> k -> l -> m -> n -> K.Type
newtype Const2 a b c d e =
  Const2 a
  deriving (Show, Eq)

instance FunctorB (Const2 a b c Covered) where
  bmap _ (Const2 a) = Const2 a

instance BareB (Const2 a b c) where
  bstrip (Const2 a) = Const2 a
  bcover (Const2 a) = Const2 a
