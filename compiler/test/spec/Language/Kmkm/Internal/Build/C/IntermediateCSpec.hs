{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.IntermediateCSpec where

import           Language.Kmkm.Internal.Build.C.IntermediateC
import qualified Language.Kmkm.Internal.Build.C.Syntax              as C
import qualified Language.Kmkm.Internal.Syntax.C.PolymorphicPointed as S

import Utility

import Test.Hspec

spec :: Spec
spec = do
  let
    tos =
      [ (["kmkm", "prim", "int"], ("kmkm_prim_int", []))
      , (["kmkm", "prim", "string"], ("kmkm_prim_string", [C.Pointer [] []]))
      ]

  describe "definition" $ do
    describe "value-bind" $ do
      describe "v" $ do
        it "foo :: int => int foo" $ do
          definition tos "spec" (FI $ S.ValueBindV ["spec", "foo"] [] $ FI $ S.TypedValue (FI $ S.Literal $ FI $ S.Integer 10 10) ["kmkm", "prim", "int"] $ S.PolymorphicPoint S.Monoparam [])
            `shouldReturn`
              [Right $ C.Definition (C.ExpressionDefinition "kmkm_prim_int" [] "spec_foo" [] (C.ExpressionInitializer $ Right $ C.Literal $ C.Integer 10 C.IntDecimal))]

        it "foo :: int -> int => int (*foo)(int)" $ do
          definition tos "spec" (FI $ S.ValueBindV ["spec", "foo"] [] $ FI $ S.TypedValue ["spec", "bar"] (FI $ S.FunctionType [["kmkm", "prim", "int"]] ["kmkm", "prim", "int"]) $ S.PolymorphicPoint S.Monoparam [S.Monoparam])
            `shouldReturn`
              [Right $ C.Definition (C.ExpressionDefinition "kmkm_prim_int" [] "spec_foo" [C.Pointer [] [], C.Function [("kmkm_prim_int", [C.Constant], Nothing, [])]] (C.ExpressionInitializer $ Right $ C.Variable "spec_bar"))]

      describe "0" $ do
        it "foo :: () -> int => int foo()" $ do
          definition tos "spec" (FI $ S.ValueBindN ["spec", "foo"] [] [] $ FI $ S.TypedValue (FI $ S.Literal $ FI $ S.Integer 10 10) ["kmkm", "prim", "int"] $ S.PolymorphicPoint S.Monoparam [])
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition "kmkm_prim_int" [] "spec_foo" [C.Function [(C.QualifiedType [] C.Void, [], Nothing, [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal]))]

        it "foo :: () -> (int -> int) => int (*foo())(int)" $ do
          definition tos "spec" (FI $ S.ValueBindN ["spec", "foo"] [] [] $ FI $ S.TypedValue ["spec", "bar"] (FI $ S.FunctionType [["kmkm", "prim", "int"]] ["kmkm", "prim", "int"]) $ S.PolymorphicPoint S.Monoparam [])
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition "kmkm_prim_int" [C.Constant] "spec_foo" [C.Function [(C.QualifiedType [] C.Void, [], Nothing, [])], C.Pointer [] [], C.Function [("kmkm_prim_int", [C.Constant], Nothing, [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Variable "spec_bar"]))]

      describe "1" $ do
        it "foo :: int -> int => int foo(int a)" $ do
          definition tos "spec" (FI $ S.ValueBindN ["spec", "foo"] [] (FI [FI ("a", ["kmkm", "prim", "int"])]) $ FI $ S.TypedValue (FI $ S.Literal $ FI $ S.Integer 10 10) ["kmkm", "prim", "int"] $ S.PolymorphicPoint S.Monoparam [S.Monoparam])
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition "kmkm_prim_int" [] "spec_foo" [C.Function [("kmkm_prim_int", [C.Constant], Just $ Right "a", [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal]))]

        it "foo :: (int -> int) -> int => int foo(int (*a)(int))" $ do
          definition tos "spec" (FI $ S.ValueBindN ["spec", "foo"] [] (FI [FI ("a", FI $ S.FunctionType (FI [["kmkm", "prim", "int"]]) ["kmkm", "prim", "int"])]) $ FI $ S.TypedValue ["spec", "bar"] ["kmkm", "prim", "int"] $ S.PolymorphicPoint S.Monoparam [S.Monoparam])
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition "kmkm_prim_int" [] "spec_foo" [C.Function [("kmkm_prim_int", [C.Constant], Just $ Right "a", [C.Pointer [C.Constant] [], C.Function [("kmkm_prim_int", [C.Constant], Nothing, [])]])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Variable "spec_bar"]))]

    describe "data-definition" $ do
      it "data bool = false | true" $ do
        definition tos "spec" (FI $ S.DataDefinition ["spec", "bool"] $ FI $ S.ForAllData [] [FI $ S.ValueConstructor "false" [], FI $ S.ValueConstructor "true" []])
          `shouldReturn`
            [ Right $ C.Declaration (C.QualifiedType [] $ C.EnumerableLiteral (Just $ C.Identifier "spec_bool_tag") [C.Identifier "false_tag", C.Identifier "true_tag"]) [] Nothing []
            , Right $ C.Declaration (C.QualifiedType [] $ C.StructureLiteral (Just $ C.Identifier "spec_bool") [C.Field (C.QualifiedType [] $ C.Enumerable (C.Identifier "spec_bool_tag")) (C.Identifier "tag") []]) [] Nothing []
            , Right $ C.Definition (C.ExpressionDefinition (C.QualifiedType [] $ C.Structure $ C.Identifier "spec_bool") [C.Constant] (C.Identifier "false") [] (C.ListInitializer [C.ExpressionInitializer $ Right $ C.Variable $ C.Identifier "false_tag"]))
            , Right $ C.Definition (C.ExpressionDefinition (C.QualifiedType [] $ C.Structure $ C.Identifier "spec_bool") [C.Constant] (C.Identifier "true") [] (C.ListInitializer [C.ExpressionInitializer $ Right $ C.Variable $ C.Identifier "true_tag"]))
            ]

      it "data solo a = solo { item :: a }" $ do
        definition tos "spec" (FI $ S.DataDefinition ["spec", "solo"] $ FI $ S.ForAllData ["a"] [FI $ S.ValueConstructor "solo" [FI $ S.Field "item" "a"]])
          `shouldReturn`
            [ Right $ C.Declaration (C.QualifiedType [] $ C.StructureLiteral (Just $ C.Identifier "spec_solo") [C.Field (C.QualifiedType [] C.Void) (C.Identifier "item") [C.Pointer [] []]]) [] Nothing []
            , Right $ C.Definition $ C.StatementDefinition (C.QualifiedType [] $ C.Structure $ C.Identifier "spec_solo") [] (C.Identifier "solo") [C.Function [(C.QualifiedType [] C.Void, [C.Constant], Just $ Right $ C.Identifier "item", [C.Pointer [C.Constant] []])]] $ Right [Right $ C.BlockStatement $ C.Return $ C.CompoundLiteral (C.QualifiedType [] $ C.Structure $ C.Identifier "spec_solo") [C.ExpressionInitializer $ Right $ C.Variable $ C.Identifier "item"]]
            ]

  describe "type" $ do
    it "kmkm.prim.int => kmkm_prim_int" $ do
      typ tos (FI ["kmkm", "prim", "int"]) `shouldReturn` ("kmkm_prim_int", [])

    it "kmkm.prim.int -> kmkm.prim.int => kmkm_prim_int (*)(kmkm_prim_int)" $ do
      typ tos (FI $ S.FunctionType [["kmkm", "prim", "int"]] ["kmkm", "prim", "int"])
        `shouldReturn`
          ("kmkm_prim_int", [C.Pointer [] [], C.Function [("kmkm_prim_int", [C.Constant], Nothing, [])]])

    it "kmkm.prim.string -> kmkm.prim.string => kmkm_prim_string (* (*)(kmkm_prim_string *))" $ do
      typ tos (FI $ S.FunctionType [["kmkm", "prim", "string"]] ["kmkm", "prim", "string"])
        `shouldReturn`
          ("kmkm_prim_string", [C.Pointer [] [C.Pointer [] [], C.Function [("kmkm_prim_string", [C.Constant], Nothing, [C.Pointer [C.Constant] []])]]])

  describe "value" $ do
    it "id @Int one => *(id(&one))" $ do
      value tos "spec"
        (FI $ S.TypedValue
          (FI $ S.Application
            (FI $ S.TypedValue
              (FI $ S.Instantiation
                (FI $ S.TypedValue
                  (FI $ S.ForAllValue "a" $ FI $ S.TypedValue "id" (FI $ S.FunctionType ["a"] "a") $ S.PolymorphicPoint S.Polyparam [S.Polyparam])
                  (FI $ S.ForAllType "a" $ FI $ S.FunctionType ["a"] "a")
                  $ S.PolymorphicPoint S.Polyparam [S.Polyparam]
                )
                ["kmkm", "prim", "int"]
              )
              (FI $ S.FunctionType [["kmkm", "prim", "int"]] ["kmkm", "prim", "int"])
              $ S.PolymorphicPoint S.Monoparam [S.Monoparam]
            )
            [FI $ S.TypedValue "one" ["kmkm", "prim", "int"] $ S.PolymorphicPoint S.Monoparam []]
          )
          ["kmkm", "prim", "int"]
          $ S.PolymorphicPoint S.Monoparam []
        )
        `shouldReturn`
          undefined
          -- C.Call
          --   (C.Cast (C.QualifiedType [] (C.TypeVariable (C.Identifier "kmkm_prim_int")), [C.Pointer [] [], C.Function [(C.QualifiedType [] (C.TypeVariable (C.Identifier "kmkm_prim_int")), [C.Constant], Nothing, [])]]) (C.Variable (C.Identifier "id")))
          --   [C.Variable (C.Identifier "one")]
