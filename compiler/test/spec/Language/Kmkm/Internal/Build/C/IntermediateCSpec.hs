{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.IntermediateCSpec where

import           Language.Kmkm.Internal.Build.C.IntermediateC
import qualified Language.Kmkm.Internal.Build.C.Syntax        as C
import qualified Language.Kmkm.Internal.Syntax                as S

import Utility

import qualified Data.Map.Strict as M
import           Test.Hspec

spec :: Spec
spec = do
  describe "intermediate" $ do
    let tos = M.fromList [(["kmkm", "prim", "int"], (([], C.TypeVariable (C.Identifier "kmkm_prim_int")), []))]

    describe "definition" $ do
      describe "v" $ do
        it "foo :: int ⇒ int foo" $ do
          definition tos "spec" (I $ S.ValueBind (I $ S.ValueBindV ["spec", "foo"] $ I $ S.TypedValue (I $ S.Literal $ I $ S.Integer 10 10) ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.ExpressionDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [] (C.ExpressionInitializer $ Right $ C.Literal $ C.Integer 10 C.IntDecimal))]

        it "foo :: int → int ⇒ int (*foo)(int)" $ do
          definition tos "spec" (I $ S.ValueBind (I $ S.ValueBindV ["spec", "foo"] $ I $ S.TypedValue ["spec", "bar"] $ I $ S.FunctionType $ I $ S.FunctionTypeN (I [["kmkm", "prim", "int"]]) ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.ExpressionDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Pointer [], C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Nothing, [])]] (C.ExpressionInitializer $ Right $ C.Variable "spec_bar"))]

      describe "0" $ do
        it "foo :: () → int ⇒ int foo()" $ do
          definition tos "spec" (I $ S.ValueBind (I $ S.ValueBindN ["spec", "foo"] [] $ I $ S.TypedValue (I $ S.Literal $ I $ S.Integer 10 10) ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal]))]

        it "foo :: () → (int → int) ⇒ int (*foo())(int)" $ do
          definition tos "spec" (I $ S.ValueBind (I $ S.ValueBindN ["spec", "foo"] [] $ I $ S.TypedValue ["spec", "bar"] $ I $ S.FunctionType $ I $ S.FunctionTypeN (I [["kmkm", "prim", "int"]]) ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])], C.Pointer [], C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Nothing, [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Variable "spec_bar"]))]

      describe "1" $ do
        it "foo :: int → int ⇒ int foo(int a)" $ do
          definition tos "spec" (I $ S.ValueBind (I $ S.ValueBindN ["spec", "foo"] (I [I ("a",  ["kmkm", "prim", "int"])]) $ I $ S.TypedValue (I $ S.Literal $ I $ S.Integer 10 10) ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Just $ Right "a", [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal]))]

        it "foo :: (int → int) → int ⇒ int foo(int (*a)(int))" $ do
          definition tos "spec" (I $ S.ValueBind (I $ S.ValueBindN ["spec", "foo"] (I [I ("a", I $ S.FunctionType $ I $ S.FunctionTypeN (I [["kmkm", "prim", "int"]]) ["kmkm", "prim", "int"])]) $ I $ S.TypedValue ["spec", "bar"] ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Just $ Right "a", [C.Pointer [C.Constant], C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Nothing, [])]])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Variable "spec_bar"]))]
