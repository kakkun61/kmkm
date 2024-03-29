{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.IntermediateC
  ( translate
  , definedVariables
  , Module
  , definition
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Build.C.Syntax as I
import qualified Language.Kmkm.Internal.Exception      as X
import           Language.Kmkm.Internal.Syntax         (Identifier (SystemIdentifier, UserIdentifier),
                                                        ModuleName (ModuleName), QualifiedIdentifier)
import qualified Language.Kmkm.Internal.Syntax         as S

import qualified Control.Exception      as E
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad          (join)
import           Data.Copointed         (Copointed (copoint))
import           Data.Functor.Identity  (Identity (Identity))
import qualified Data.List.NonEmpty     as N
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe, mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Typeable          as Y
import           GHC.Generics           (Generic)
import Data.Functor.With (MayHave)
import qualified Data.Functor.With as W

type Module = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

type Definition = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

type Type = S.Type 'S.NameResolved 'S.Uncurried

type Value = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

type ProcedureStep = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue

translate
  :: ( MonadThrow m
     , Functor f
     , Foldable f
     , Copointed f
     , MayHave S.Location f
     )
  => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver])
  -> f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue f)
  -> m I.File
translate = module'

module' :: (MonadThrow m, Functor f, Foldable f, Copointed f, MayHave S.Location f) => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f (Module f) -> m I.File
module' definedVariables m =
  let S.Module n _ ds = copoint m
  in I.File (moduleName $ copoint n) . join <$> traverse (definition definedVariables n) (copoint ds)

-- |
-- @
--                  |   the number of constructors
--                  +---------+-----------+-----------
--                  |    0    |     1     |    n>1
-- -----------+-----+---------+-----------+-----------
--            |  0  | invalid | has a tag | has a tag
-- the number |     |         | value     | value
-- of fields  +-----+---------+-----------+-----------
--            | n>0 | invalid | no tags   | has a tag
--            |     |         | function  | function
-- @
definition :: (MonadThrow m, Functor f, Foldable f, Copointed f, MayHave S.Location f) => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f ModuleName -> f (Definition f) -> m [Either Text I.Element]
definition definedVariables n d =
  case copoint d of
    S.DataDefinition i cs ->
      pure $
        foldMap
          (fmap Right)
            [ tagEnum
            , [structure]
            , I.Definition . constructor (length cs_) <$> cs_
            ]
      where
        cs_ = copoint cs
        tagEnumIdent i = I.Identifier $ qualifiedIdentifierText i <> "_tag"
        tagEnumType = ([], I.Enumerable $ tagEnumIdent i)
        tagEnum =
          case cs_ of
            [c]
              | S.ValueConstructor _ fs <- copoint c
              , _:_ <- copoint fs ->
                []
            _ -> [I.Declaration ([], I.EnumerableLiteral (Just $ tagEnumIdent i) $ tagEnumIdent . (\(S.ValueConstructor i _) -> i) . copoint <$> cs_) [] Nothing []]
        structType = ([], I.Structure $ qualifiedIdentifier i)
        structure =
          I.Declaration ([], I.StructureLiteral (Just $ qualifiedIdentifier i) fields) [] Nothing []
          where
            fields =
              case cs_ of
                [c]
                  | S.ValueConstructor _ fs <- copoint c
                  , fs_@(_:_) <- copoint fs ->
                    field <$> fs_
                _ -> I.Field tagEnumType "tag" [] : [ I.Field ([], I.Union $ constructor <$> cs_) "body" [] | hasFields ]
            field :: Copointed f => f (S.Field 'S.NameResolved 'S.Uncurried l et ev f) -> I.Field
            field f =
              let
                S.Field i t = copoint f
                (t', ds) = typ definedVariables t
              in
                I.Field t' (qualifiedIdentifier i) ds
            constructor :: Copointed f => f (S.ValueConstructor 'S.NameResolved 'S.Uncurried 'S.LambdaLifted S.EmbeddedCType S.EmbeddedCValue f) -> I.Field
            constructor c =
              let S.ValueConstructor i fs = copoint c
              in I.Field ([], I.StructureLiteral Nothing $ field <$> copoint fs) (qualifiedIdentifier i) []
            hasFields =
              or $ go <$> cs_
              where
                go :: Copointed f => f (S.ValueConstructor 'S.NameResolved 'S.Uncurried 'S.LambdaLifted S.EmbeddedCType S.EmbeddedCValue f) -> Bool
                go c =
                  let S.ValueConstructor _ fs = copoint c
                  in not $ null $ copoint fs
        constructor :: Copointed f => Int -> f (S.ValueConstructor 'S.NameResolved 'S.Uncurried 'S.LambdaLifted et ev f) -> I.Definition
        constructor n c =
          case copoint c of
            S.ValueConstructor c' fs ->
              case copoint fs of
                [] -> I.ExpressionDefinition structType [I.Constant] (qualifiedIdentifier c') [] $ I.ListInitializer [I.ExpressionInitializer $ Right $ I.Variable $ tagEnumIdent c']
                fs_ ->
                  I.StatementDefinition structType [] (qualifiedIdentifier c') [I.Function $ parameter <$> fs_] $ Right [blockItem]
                  where
                    parameter :: Copointed f => f (S.Field 'S.NameResolved 'S.Uncurried 'S.LambdaLifted et ev f) -> (I.QualifiedType, [I.VariableQualifier], Maybe (Either a I.Identifier), [I.Deriver])
                    parameter p =
                      let
                        S.Field i t = copoint p
                        (t', ds) = typ definedVariables t
                      in
                        (t', [I.Constant], Just $ Right $ qualifiedIdentifier i, constantDeriver <$> ds)
                    blockItem = Right $ I.BlockStatement $ I.Return $ I.CompoundLiteral structType arguments
                    arguments =
                      case (n, fs_) of
                        (1, _:_) -> argument <$> fs_
                        _        -> I.ExpressionInitializer (Right $ I.Variable $ tagEnumIdent c') : (argument <$> fs_)
                    argument :: Copointed f => f (S.Field 'S.NameResolved 'S.Uncurried 'S.LambdaLifted et ev f) -> I.Initializer
                    argument f | S.Field i _ <- copoint f = I.ExpressionInitializer $ Right $ I.Variable $ qualifiedIdentifier i
    S.ValueBind b ->
      case copoint b of
        S.ValueBindV i v -> do
          let
            S.TypedValue _ t = copoint v
            (t', ds) = typ definedVariables t
          v' <- value definedVariables n v
          pure [Right $ I.Definition $ I.ExpressionDefinition t' [] (qualifiedIdentifier i) (ds ++ derivers definedVariables t) $ I.ExpressionInitializer $ Right v']
        S.ValueBindN i is v -> do
          b <- bindTermN definedVariables n i is v
          pure [Right b]
    S.ForeignValueBind i e t ->
      case copoint t of
        S.TypeVariable {} ->
          case S.toIdentity e of
            Identity (S.EmbeddedCValue (Identity n) (Identity []) (Identity b)) ->
              pure
                [ Left n
                , let (t', ds) = typ definedVariables t
                  in Right $ I.Definition $ I.ExpressionDefinition t' [I.Constant] (qualifiedIdentifier i) (constantDeriver <$> ds) $ I.ExpressionInitializer $ Left b
                ]
            Identity (S.EmbeddedCValue _ ps _) -> throw $ EmbeddedParameterMismatchException 0 (fromIntegral $ length ps) (W.mayGet e)
        S.FunctionType t' | (S.FunctionTypeN ts r) <- copoint t' ->
          case copoint e of
            S.EmbeddedCValue n ps b
              | ps_ <- copoint ps
              , ts_ <- copoint ts
              , length ps_ == length ts_ ->
                let
                  r' =
                    case copoint r of
                      S.ProcedureType t -> t
                      _                 -> r
                  ps' =
                    zipWith go ps_ ts_ <$ ps
                    where
                      go p t =
                        let p_ = copoint p
                        in (Left p_ <$ p, t) <$ p
                  (r'', ds) = typ definedVariables r'
                in
                  pure
                    [ Left $ copoint n
                    , Right $
                        I.Definition $
                          I.StatementDefinition r'' [] (qualifiedIdentifier i) (ds ++ [I.Function (parameters definedVariables ps')]) $ Left $ copoint b
                    ]
              | otherwise -> throw $ EmbeddedParameterMismatchException (fromIntegral $ length $ copoint ts) (fromIntegral $ length $ copoint ps) $ W.mayGet e
        S.ProcedureType {} ->
          let (t', ds) = typ definedVariables t
          in
            case copoint e of
              S.EmbeddedCValue n ps b | [] <- copoint ps ->
                pure
                  [ Left $ copoint n
                  , Right $
                      I.Definition $
                        I.StatementDefinition t' [] (qualifiedIdentifier i) (I.Function [(([], I.Void), [], Nothing, [])] : ds) $ Left $ copoint b
                  ]
              S.EmbeddedCValue _ ps _ -> throw $ EmbeddedParameterMismatchException 0 (fromIntegral $ length $ copoint ps) $ W.mayGet e
        _ -> X.unreachable
    S.TypeBind i t ->
      pure [Right $ I.TypeDefinition (Right $ typ definedVariables t) $ qualifiedIdentifier i]
    S.ForeignTypeBind i e ->
      case copoint e of
        S.EmbeddedCType n b ->
          pure
            [ Left $ copoint n
            , Right $ I.TypeDefinition (Left $ copoint b) (qualifiedIdentifier i)
            ]

bindTermN :: (MonadThrow m, Functor f, Foldable f, Copointed f, MayHave S.Location f) => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f ModuleName -> f S.QualifiedIdentifier -> f [f (f S.QualifiedIdentifier, f (Type f))] -> f (Value f) -> m I.Element
bindTermN definedVariables n i ps v = do
  let
    ps_ = copoint ps
    S.TypedValue _ t = copoint v
    ps' = (go <$> ps_) <$ ps
    go p =
      let
        (i, t) = copoint p
        i_ = copoint i
      in (Right i_ <$ i, t) <$ p
    (t', ds) = typ definedVariables t
    returnsPointer = any (\case { I.Pointer {} -> True; _ -> False }) ds
  v' <- value definedVariables n v
  pure $
    I.Definition $
      I.StatementDefinition
        t'
        [ I.Constant | returnsPointer ]
        (qualifiedIdentifier i)
        (I.Function (parameters definedVariables ps') : ds ++ derivers definedVariables t) $
          Right [Right $ I.BlockStatement $ I.Return v']

parameters
  :: ( Functor f
     , Copointed f
     )
  => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver])
  -> f [f (f (Either Text QualifiedIdentifier), f (Type f))]
  -> [(([I.TypeQualifier], I.Type), [I.VariableQualifier], Maybe (Either Text I.Identifier), [I.Deriver])]
parameters definedVariables ps =
  case copoint ps of
    []  -> [(([], I.Void), [], Nothing, [])]
    ps_ -> parameter <$> ps_
  where
    parameter p =
      let
        (i, t) = copoint p
        i_ = copoint i
        (t', ds) = typ definedVariables t
      in (t', [I.Constant], Just $ qualifiedIdentifier . (<$ i) <$> i_, constantDeriver <$> ds ++ derivers definedVariables t)

elementStatement :: I.Element -> I.BlockElement
elementStatement (I.Declaration t qs i ds) = I.BlockDeclaration t qs i ds
elementStatement (I.Definition d)          = I.BlockDefinition d
elementStatement (I.TypeDefinition t i)    = I.BlockTypeDefinition t i

qualifiedIdentifier :: Copointed f => f QualifiedIdentifier -> I.Identifier
qualifiedIdentifier = I.Identifier . qualifiedIdentifierText

identifierText :: S.Identifier -> Text
identifierText (UserIdentifier t)     = t
identifierText (SystemIdentifier t n) = T.pack $ '_' : t : show n

qualifiedIdentifierText :: Copointed f => f QualifiedIdentifier -> Text
qualifiedIdentifierText i =
  case copoint i of
    S.GlobalIdentifier "main" i -> identifierText i
    S.GlobalIdentifier m i      -> moduleName m <> "_" <> identifierText i
    S.LocalIdentifier i         -> identifierText i

moduleName :: ModuleName -> Text
moduleName (ModuleName n) = T.intercalate "_" $ N.toList n

value :: (MonadThrow m, Functor f, Foldable f, Copointed f, MayHave S.Location f) => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f ModuleName -> f (Value f) -> m I.Expression
value definedVariables n v =
  let S.TypedValue v' _ = copoint v
  in
    case copoint v' of
      S.Variable i -> pure $ I.Variable $ qualifiedIdentifier i
      S.Literal l -> pure $ I.Literal $ literal l
      S.Application a ->
        let S.ApplicationN v vs = copoint a
        in I.Call <$> value definedVariables n v <*> traverse (value definedVariables n) (copoint vs)
      S.Procedure ps -> I.StatementExpression . join <$> traverse (fmap (fmap Right) . procedureStep definedVariables n) (N.toList $ copoint ps)
      S.Let ds v1 -> do
        let definedVariables' = M.fromList (mapMaybe definedVariable $ copoint ds) `M.union` definedVariables
        es <- join <$> traverse (definition definedVariables' n) (copoint ds)
        v1' <- value definedVariables n v1
        pure $ I.StatementExpression $ (fmap elementStatement <$> es) ++ [Right $ I.BlockStatement $ I.ExpressionStatement v1']
      S.ForAll _ v1 -> value definedVariables n v1
      S.Function _ -> X.unreachable
      S.TypeAnnotation _ -> X.unreachable

literal :: Copointed f => f S.Literal -> I.Literal
literal l =
  case copoint l of
    S.Integer i b ->
      I.Integer i $
        case b of
          2  -> I.IntBinary
          8  -> I.IntOctal
          10 -> I.IntDecimal
          16 -> I.IntHexadecimal
          _  -> error $ "literal: base: " ++ show b
    S.Fraction s f e b ->
      I.Fraction s f e $
        case b of
          10 -> I.FractionDecimal
          16 -> I.FractionHexadecimal
          _  -> error $ "literal: base " ++ show b
    S.String s -> I.String s -- XXX バックスラッシュでバイナリー表記にした方がいいかも

procedureStep :: (MonadThrow m, Functor f, Foldable f, Copointed f, MayHave S.Location f) => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f ModuleName -> f (ProcedureStep f) -> m [I.BlockElement]
procedureStep definedVariables n s =
  case copoint s of
    S.BindProcedureStep i v -> do
      let S.TypedValue _ t = copoint v
      v' <- value definedVariables n v
      pure
        [ let (t', ds) = typ definedVariables t
          in I.BlockDeclaration t' [I.Constant] (Just $ qualifiedIdentifier i) $ constantDeriver <$> ds
        , I.BlockStatement $ I.ExpressionStatement $ I.Assign (qualifiedIdentifier i) v'
        ]
    S.CallProcedureStep v -> do
      v' <- value definedVariables n v
      pure [I.BlockStatement $ I.ExpressionStatement v']

typ :: Copointed f => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f (Type f) -> (I.QualifiedType, [I.Deriver])
typ definedVariables t =
  case copoint t of
    S.TypeVariable n -> fromMaybe (([], I.Void), [I.Pointer []]) $ M.lookup (copoint n) definedVariables
    S.FunctionType t' | (S.FunctionTypeN _ t) <- copoint t' -> typ definedVariables t
    S.ProcedureType t -> typ definedVariables t
    _ -> undefined

derivers :: Copointed f => Map QualifiedIdentifier (I.QualifiedType, [I.Deriver]) -> f (Type f) -> [I.Deriver]
derivers definedVariables t =
  case copoint t of
    S.FunctionType t' | (S.FunctionTypeN ts _) <- copoint t' ->
        [ I.Pointer []
        , I.Function $ go <$> copoint ts
        ]
        where
          go t =
            let (t', ds) = typ definedVariables t
            in (t', [I.Constant], Nothing, constantDeriver <$> ds ++ derivers definedVariables t)
    _ -> []

definedVariables
  :: Copointed f
  => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue f)
  -> Map QualifiedIdentifier (I.QualifiedType, [I.Deriver])
definedVariables m =
  let S.Module _ _ ds = copoint m
  in M.fromList $ mapMaybe definedVariable $ copoint ds

constantDeriver :: I.Deriver -> I.Deriver
constantDeriver (I.Pointer qs) = I.Pointer $ I.Constant : qs
constantDeriver d              = d

definedVariable
  :: Copointed f
  => f (S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed S.EmbeddedCType S.EmbeddedCValue f)
  -> Maybe (S.QualifiedIdentifier, (I.QualifiedType, [I.Deriver]))
definedVariable d =
  case copoint d of
    S.DataDefinition i _  -> Just (copoint i, (([], I.Structure $ qualifiedIdentifier i), []))
    S.TypeBind i _        -> Just (copoint i, (([], I.TypeVariable $ qualifiedIdentifier i), []))
    S.ForeignTypeBind i _ -> Just (copoint i, (([], I.TypeVariable $ qualifiedIdentifier i), []))
    _                     -> Nothing

data Exception
  = EmbeddedParameterMismatchException { expected :: Word, actual :: Word, location :: Maybe S.Location }
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
