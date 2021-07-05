{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Build.C.IntermediateC
  ( translate
  , typeOrigins
  , Module
  , TypeOrigin (..)
  , definition
  ) where

import qualified Language.Kmkm.Build.C.Syntax as I
import           Language.Kmkm.Exception      (unreachable)
import qualified Language.Kmkm.Exception      as X
import           Language.Kmkm.Syntax         (Identifier (SystemIdentifier, UserIdentifier), ModuleName (ModuleName),
                                               QualifiedIdentifier)
import qualified Language.Kmkm.Syntax         as S

import           Data.Functor.Identity (Identity (Identity, runIdentity))
import qualified Data.List.NonEmpty    as N
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

type Module = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed Identity

type Definition = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed Identity

type Type = S.Type 'S.NameResolved 'S.Uncurried Identity

type Value = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed Identity

type Literal = S.Literal 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed Identity

type ProcedureStep = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed Identity

translate :: S.HasPosition f => Map QualifiedIdentifier TypeOrigin -> f ( S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f) -> ([S.CHeader], I.File)
translate typeOrigins = module' typeOrigins . S.prune

module' :: Map QualifiedIdentifier TypeOrigin -> Identity Module -> ([S.CHeader], I.File)
module' typeOrigins (Identity (S.Module n _ ds)) = (header =<< ds, I.File (moduleName $ runIdentity n) $ definition typeOrigins n =<< ds)

header :: Identity Definition -> [S.CHeader]
header (Identity (S.ForeignTypeBind _ hs _))    = hs
header (Identity (S.ForeignValueBind _ hs _ _)) = hs
header _                                        = []

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
definition :: Map QualifiedIdentifier TypeOrigin -> Identity ModuleName -> Identity Definition -> [I.Element]
definition typeOrigins _ (Identity (S.DataDefinition (Identity i) cs)) =
  mconcat
    [ tagEnum
    , [structure]
    , I.Definition . constructor (length cs) <$> cs
    ]
  where
    tagEnumIdent i = I.Identifier $ qualifiedIdentifierText i <> "_tag"
    tagEnumType = ([], I.Enumerable $ tagEnumIdent i)
    tagEnum =
      case cs of
        [(_, _:_)] -> []
        _          -> [I.Declaration ([], I.EnumerableLiteral (Just $ tagEnumIdent i) $ tagEnumIdent . runIdentity . fst <$> cs) [] Nothing []]
    structType = ([], I.Structure $ qualifiedIdentifier i)
    structure =
      I.Declaration ([], I.StructureLiteral (Just $ qualifiedIdentifier i) fields) [] Nothing []
      where
        fields =
          case cs of
            [(_, fs@(_:_))] -> field <$> fs
            _ ->
              I.Field tagEnumType "tag"
                :
                  if hasFields
                    then [I.Field ([], I.Union $ constructor <$> cs) "body"]
                    else []
        field (Identity i, t) = I.Field (typ typeOrigins t) $ qualifiedIdentifier i
        constructor (Identity i, fs) = I.Field ([], I.StructureLiteral Nothing $ field <$> fs) $ qualifiedIdentifier i
        hasFields = or $ go <$> cs where go (_, fs) = not $ null fs
    constructor _ (Identity c, []) = I.ExpressionDefinition structType [I.Constant] (qualifiedIdentifier c) [] $ I.ListInitializer [I.ExpressionInitializer $ I.Variable $ tagEnumIdent c]
    constructor n (Identity c, fs) =
      I.StatementDefinition structType [] (qualifiedIdentifier c) [I.Function $ parameter <$> fs] [blockItem]
      where
        parameter (Identity i, t) = (typ typeOrigins t, [I.Constant], Just $ qualifiedIdentifier i, [])
        blockItem = I.BlockStatement $ I.Return $ I.CompoundLiteral structType arguments
        arguments =
          case (n, fs) of
            (1, _:_) -> argument <$> fs
            _        -> I.ExpressionInitializer (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (Identity i, _) = I.ExpressionInitializer $ I.Variable $ qualifiedIdentifier i
definition typeOrigins n (Identity (S.ValueBind (S.ValueBindV (Identity i) v@(Identity(S.TypedValue _ t))))) =
  [I.Definition $ I.ExpressionDefinition (typ typeOrigins t) [] (qualifiedIdentifier i) (deriver typeOrigins t) $ I.ExpressionInitializer $ value typeOrigins n v]
definition typeOrigins n (Identity (S.ValueBind (S.ValueBindN i is v))) =
  [bindTermN typeOrigins n i is v]
definition _ _ (Identity (S.ForeignValueBind _ _ (Identity (S.CDefinition c)) _)) =
  [I.Embedded $ I.C c]
definition typeOrigins _ (Identity (S.TypeBind (Identity i) t)) =
  [I.TypeDefinition (typ typeOrigins t) $ qualifiedIdentifier i]
definition _ _ (Identity (S.ForeignTypeBind _ _ (Identity (S.CDefinition c)))) =
  [I.Embedded $ I.C c]

bindTermN :: Map QualifiedIdentifier TypeOrigin -> Identity ModuleName -> Identity S.QualifiedIdentifier -> [(Identity S.QualifiedIdentifier, Identity Type)] -> Identity Value -> I.Element
bindTermN typeOrigins n (Identity i) ps v@(Identity (S.TypedValue _ t)) =
  I.Definition $ I.StatementDefinition (typ typeOrigins t) [] (qualifiedIdentifier i) (I.Function (parameters ps) : deriverRoot typeOrigins t) [I.BlockStatement $ I.Return (value typeOrigins n v)]
  where
    parameters [] = [(([], I.Void), [], Nothing, [])]
    parameters ps = parameter <$> ps
    parameter (Identity i, t) = (typ typeOrigins t, case t of { Identity S.FunctionType {} -> []; _ -> [I.Constant] }, Just $ qualifiedIdentifier i, deriver typeOrigins t)

elementStatement :: I.Element -> I.BlockElement
elementStatement (I.Declaration t qs i ds) = I.BlockDeclaration t qs i ds
elementStatement (I.Definition d)          = I.BlockDefinition d
elementStatement (I.TypeDefinition t i)    = I.BlockTypeDefinition t i
elementStatement (I.Embedded c)            = I.BlockEmbed c

qualifiedIdentifier :: QualifiedIdentifier -> I.Identifier
qualifiedIdentifier = I.Identifier . qualifiedIdentifierText

identifierText :: S.Identifier -> Text
identifierText (UserIdentifier t)     = t
identifierText (SystemIdentifier t n) = T.pack $ '_' : t : show n

qualifiedIdentifierText :: QualifiedIdentifier -> Text
qualifiedIdentifierText (S.GlobalIdentifier "main" i) = identifierText i
qualifiedIdentifierText (S.GlobalIdentifier m i)      = moduleName m <> "_" <> identifierText i
qualifiedIdentifierText (S.LocalIdentifier i)         = identifierText i

moduleName :: ModuleName -> Text
moduleName (ModuleName n) = T.intercalate "_" $ N.toList n

value :: Map QualifiedIdentifier TypeOrigin -> Identity ModuleName -> Identity Value -> I.Expression
value _ _ (Identity (S.TypedValue (Identity (S.Variable i)) _)) = I.Variable $ qualifiedIdentifier i
value _ _ (Identity (S.TypedValue (Identity (S.Literal l)) _)) = I.Literal $ literal l
value typeOrigins n (Identity (S.TypedValue (Identity (S.Application (S.ApplicationN v vs))) _)) = I.Call (value typeOrigins n v) $ value typeOrigins n <$> vs
value typeOrigins n (Identity (S.TypedValue (Identity (S.Procedure ps)) _)) = I.StatementExpression $ I.Block $ procedureStep typeOrigins n =<< N.toList ps
value _ _ (Identity (S.TypedValue (Identity (S.TypeAnnotation _)) _)) = unreachable
value typeOrigins n (Identity (S.TypedValue (Identity (S.Let ds v)) _)) =
  let typeOrigins' = M.fromList (mapMaybe typeOrigin ds) `M.union` typeOrigins
  in I.StatementExpression $ I.Block $ (elementStatement <$> (definition typeOrigins' n =<< ds)) ++ [I.BlockStatement (I.ExpressionStatement $ value typeOrigins' n v)]

literal :: Literal -> I.Literal
literal (S.Integer i b) =
  I.Integer i $
    case b of
      2  -> I.IntBinary
      8  -> I.IntOctal
      10 -> I.IntDecimal
      16 -> I.IntHexadecimal
      _  -> error $ "literal: base: " ++ show b
literal (S.Fraction s f e b) =
  I.Fraction s f e $
    case b of
      10 -> I.FractionDecimal
      16 -> I.FractionHexadecimal
      _  -> error $ "literal: base " ++ show b
literal l = error (show l)

procedureStep :: Map QualifiedIdentifier TypeOrigin -> Identity ModuleName -> Identity ProcedureStep -> [I.BlockElement]
procedureStep typeOrigins n (Identity (S.BindProcedure (Identity i) v@(Identity (S.TypedValue _ t)))) =
  [ I.BlockDeclaration (typ typeOrigins t) [I.Constant] (Just $ qualifiedIdentifier i) []
  , I.BlockStatement $ I.ExpressionStatement $ I.Assign (qualifiedIdentifier i) (value typeOrigins n v)
  ]
procedureStep typeOrigins n (Identity (S.TermProcedure v)) =
  [I.BlockStatement $ I.ExpressionStatement $ value typeOrigins n v]

typ :: Map QualifiedIdentifier TypeOrigin -> Identity Type -> I.QualifiedType
typ typeOrigins (Identity (S.TypeVariable (Identity n))) =
  case M.lookup n typeOrigins of
    Just AliasType -> ([], I.TypeVariable $ qualifiedIdentifier n)
    Just DataType  -> ([], I.Structure $ qualifiedIdentifier n)
    Nothing        -> X.unreachable
typ typeOrigins (Identity (S.FunctionType (S.FunctionTypeN _ t))) = typ typeOrigins t
typ _ t = error $ show t

deriverRoot :: Map QualifiedIdentifier TypeOrigin -> Identity Type -> [I.Deriver]
deriverRoot typeOrigins (Identity (S.FunctionType (S.FunctionTypeN ts _))) = deriverFunction typeOrigins True ts
deriverRoot _ _                                                            = []

deriver :: Map QualifiedIdentifier TypeOrigin -> Identity Type -> [I.Deriver]
deriver typeOrigins (Identity (S.FunctionType (S.FunctionTypeN ts _))) = deriverFunction typeOrigins False ts
deriver _ _                                                            = []

deriverFunction :: Map QualifiedIdentifier TypeOrigin -> Bool -> [Identity Type] -> [I.Deriver]
deriverFunction typeOrigins root ts =
  [ I.Pointer if root then [] else [I.Constant]
  , I.Function $ go <$> ts
  ]
  where
    go t = (typ typeOrigins t, [I.Constant], Nothing, deriver typeOrigins t)

data TypeOrigin
  = AliasType
  | DataType
  deriving (Show, Read, Eq, Ord, Generic)

typeOrigins :: S.HasPosition f => f (S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f) -> Map QualifiedIdentifier TypeOrigin
typeOrigins m =
  let S.Module _ _ ds = S.item m
  in M.fromList $ mapMaybe typeOrigin ds

typeOrigin :: S.HasPosition f => f (S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed f) -> Maybe (S.QualifiedIdentifier, TypeOrigin)
typeOrigin d =
  case S.item d of
    S.DataDefinition i _    -> Just (S.item i, DataType)
    S.TypeBind i _          -> Just (S.item i, AliasType)
    S.ForeignTypeBind i _ _ -> Just (S.item i, AliasType)
    _                       -> Nothing
