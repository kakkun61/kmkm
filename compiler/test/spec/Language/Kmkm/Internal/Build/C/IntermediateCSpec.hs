{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.IntermediateCSpec where

import           Language.Kmkm.Internal.Build.C.IntermediateC
import qualified Language.Kmkm.Internal.Build.C.Syntax        as C
import qualified Language.Kmkm.Internal.Syntax                as S

import           Barbies.Bare.Layered
import           Data.Functor.Identity
import qualified Data.Map.Strict               as M
import           Language.Kmkm.Internal.Syntax
import           Test.Hspec

spec :: Spec
spec = do
  describe "intermediate" $ do
    let tos = M.fromList [(["kmkm", "prim", "int"], (([], C.TypeVariable (C.Identifier "kmkm_prim_int")), []))]

    describe "definition" $ do
      describe "v" $ do
        it "foo :: int ⇒ int foo" $ do
          definition tos "spec" (cover $ S.ValueBind (S.ValueBindV ["spec", "foo"] $ S.TypedValue (S.Literal $ S.Integer 10 10) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.ExpressionDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [] (C.ExpressionInitializer $ Right $ C.Literal $ C.Integer 10 C.IntDecimal))]

        it "foo :: int → int ⇒ int (*foo)(int)" $ do
          definition tos "spec" (cover $ S.ValueBind (S.ValueBindV ["spec", "foo"] $ S.TypedValue (S.Variable ["spec", "bar"]) $ FunctionType $ FunctionTypeN [TypeVariable ["kmkm", "prim", "int"]] $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.ExpressionDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Pointer [], C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Nothing, [])]] (C.ExpressionInitializer $ Right $ C.Variable "spec_bar"))]

      describe "0" $ do
        it "foo :: () → int ⇒ int foo()" $ do
          definition tos "spec" (cover $ S.ValueBind (S.ValueBindN ["spec", "foo"] [] $ S.TypedValue (S.Literal $ S.Integer 10 10) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal]))]

        it "foo :: () → (int → int) ⇒ int (*foo())(int)" $ do
          definition tos "spec" (cover $ S.ValueBind (S.ValueBindN ["spec", "foo"] [] $ S.TypedValue (S.Variable ["spec", "bar"]) $ FunctionType $ FunctionTypeN [TypeVariable ["kmkm", "prim", "int"]] $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.Void), [], Nothing, [])], C.Pointer [], C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Nothing, [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Variable "spec_bar"]))]

      describe "1" $ do
        it "foo :: int → int ⇒ int foo(int a)" $ do
          definition tos "spec" (cover $ S.ValueBind (S.ValueBindN ["spec", "foo"] [("a", TypeVariable ["kmkm", "prim", "int"])] $ S.TypedValue (S.Literal $ S.Integer 10 10) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Just $ Right "a", [])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Literal $ C.Integer 10 C.IntDecimal]))]

        it "foo :: (int → int) → int ⇒ int foo(int (*a)(int))" $ do
          definition tos "spec" (cover $ S.ValueBind (S.ValueBindN ["spec", "foo"] [("a", FunctionType $ FunctionTypeN [TypeVariable ["kmkm", "prim", "int"]] $ TypeVariable ["kmkm", "prim", "int"])] $ S.TypedValue (S.Variable ["spec", "bar"]) $ TypeVariable ["kmkm", "prim", "int"]))
            `shouldReturn`
              [Right $ C.Definition (C.StatementDefinition ([], C.TypeVariable "kmkm_prim_int") [] "spec_foo" [C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Just $ Right "a", [C.Pointer [C.Constant], C.Function [(([], C.TypeVariable "kmkm_prim_int"), [C.Constant], Nothing, [])]])]] (Right [Right $ C.BlockStatement $ C.Return $ C.Variable "spec_bar"]))]

cover :: BareB b => b Bare Identity -> Identity (b Covered Identity)
cover = Identity . bcoverWith Identity
