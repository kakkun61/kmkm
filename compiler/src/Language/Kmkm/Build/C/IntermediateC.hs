{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Build.C.IntermediateC
  ( translate
  , Module
  , definition
  ) where

import qualified Language.Kmkm.Build.C.Syntax as I
import           Language.Kmkm.Config         (Config (Config, typeMap))
import qualified Language.Kmkm.Config         as C
import           Language.Kmkm.Exception      (unreachable)
import qualified Language.Kmkm.Primitive      as P
import           Language.Kmkm.Syntax         (Identifier (SystemIdentifier, UserIdentifier), ModuleName (ModuleName),
                                               QualifiedIdentifier)
import qualified Language.Kmkm.Syntax         as S

import qualified Data.List.NonEmpty as N
import           Data.Text          (Text)
import qualified Data.Text          as T

type Module = S.Module 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type Definition = S.Definition 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type Type = S.Type 'S.NameResolved 'S.Uncurried

type Value = S.Value 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type Literal = S.Literal 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

type ProcedureStep = S.ProcedureStep 'S.NameResolved 'S.Uncurried 'S.LambdaLifted 'S.Typed

translate :: Config -> Module -> ([S.CHeader], I.File)
translate = module'

module' :: Config -> Module -> ([S.CHeader ], I.File)
module' config (S.Module n _ ms) = (header =<< ms, I.File (moduleName n) $ definition config n =<< ms)

header :: Definition -> [S.CHeader]
header (S.ForeignValueBind _ hs _ _) = hs
header _                             = []

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
definition :: Config -> ModuleName -> Definition -> [I.Element]
definition config _ (S.DataDefinition i cs) =
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
        _          -> [I.Declaration ([], I.EnumerableLiteral (Just $ tagEnumIdent i) $ tagEnumIdent . fst <$> cs) [] Nothing []]
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
        field (i, t) = I.Field (typ config t) $ qualifiedIdentifier i
        constructor (i, fs) = I.Field ([], I.StructureLiteral Nothing $ field <$> fs) $ qualifiedIdentifier i
        hasFields = or $ go <$> cs where go (_, fs) = not $ null fs
    constructor _ (c, []) = I.ExpressionDefinition structType [I.Constant] (qualifiedIdentifier c) [] $ I.ListInitializer [I.ExpressionInitializer $ I.Variable $ tagEnumIdent c]
    constructor n (c, fs) =
      I.StatementDefinition structType [] (qualifiedIdentifier c) [I.Function $ parameter <$> fs] [blockItem]
      where
        parameter (i, t) = (typ config t, [I.Constant], Just $ qualifiedIdentifier i, [])
        blockItem = I.BlockStatement $ I.Return $ I.CompoundLiteral structType arguments
        arguments =
          case (n, fs) of
            (1, _:_) -> argument <$> fs
            _        -> I.ExpressionInitializer (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (i, _) = I.ExpressionInitializer $ I.Variable $ qualifiedIdentifier i
definition config n (S.ValueBind (S.ValueBindV i v@(S.TypedTerm _ t))) =
  [I.Definition $ I.ExpressionDefinition (typ config t) [] (qualifiedIdentifier i) (deriver config t) $ I.ExpressionInitializer $ term config n v]
definition config n (S.ValueBind (S.ValueBindN i is v)) =
  [bindTermN config n i is v]
definition _ _ (S.ForeignValueBind _ _ (S.CDefinition c) _) =
  [I.Embed $ I.C c]
definition config _ (S.TypeBind i t) =
  [I.TypeDefinition (typ config t) $ qualifiedIdentifier i]

bindTermN :: Config -> ModuleName -> S.QualifiedIdentifier -> [(S.QualifiedIdentifier, Type)] -> Value -> I.Element
bindTermN config n i ps v@(S.TypedTerm _ t) =
  I.Definition $ I.StatementDefinition (typ config t) [] (qualifiedIdentifier i) (I.Function (parameters ps) : deriverRoot config t) [I.BlockStatement $ I.Return (term config n v)]
  where
    parameters [] = [(([], I.Void), [], Nothing, [])]
    parameters ps = parameter <$> ps
    parameter (i, t) = (typ config t, case t of { S.FunctionType {} -> []; _ -> [I.Constant] }, Just $ qualifiedIdentifier i, deriver config t)

elementStatement :: I.Element -> I.BlockElement
elementStatement (I.Declaration t qs i ds) = I.BlockDeclaration t qs i ds
elementStatement (I.Definition d)          = I.BlockDefinition d
elementStatement (I.TypeDefinition t i)    = I.BlockTypeDefinition t i
elementStatement (I.Embed c)               = I.BlockEmbed c

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

term :: Config -> ModuleName -> Value -> I.Expression
term _ _ (S.TypedTerm (S.Variable i) _)                             = I.Variable $ qualifiedIdentifier i
term _ _ (S.TypedTerm (S.Literal l) _)                              = I.Literal $ literal l
term config n (S.TypedTerm (S.Application (S.ApplicationN v vs)) _) = I.Call (term config n v) $ term config n <$> vs
term config n (S.TypedTerm (S.Procedure ps) _)                      = I.StatementExpression $ I.Block $ procedureStep config n =<< N.toList ps
term _ _ (S.TypedTerm (S.TypeAnnotation _) _)                       = unreachable
term config n (S.TypedTerm (S.Let ds v) _) = I.StatementExpression $ I.Block $ (elementStatement <$> (definition config n =<< ds)) ++ [I.BlockStatement (I.ExpressionStatement $ term config n v)]

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

procedureStep :: Config -> ModuleName -> ProcedureStep -> [I.BlockElement]
procedureStep config n (S.BindProcedure i v@(S.TypedTerm _ t)) =
  [ I.BlockDeclaration (typ config t) [I.Constant] (Just $ qualifiedIdentifier i) []
  , I.BlockStatement $ I.ExpressionStatement $ I.Assign (qualifiedIdentifier i) (term config n v)
  ]
procedureStep config n (S.TermProcedure v) =
  [I.BlockStatement $ I.ExpressionStatement $ term config n v]

typ :: Config -> Type -> I.QualifiedType
typ Config { typeMap } (S.TypeVariable n) =
  get typeMap
  where
    get | n == P.int   = I.readCType . C.int
        | n == P.uint  = I.readCType . C.uint
        | n == P.byte  = I.readCType . C.byte
        | n == P.frac  = I.readCType . C.frac
        | n == P.frac2 = I.readCType . C.frac2
        | otherwise    = const ([], I.Structure $ qualifiedIdentifier n)
typ c (S.FunctionType (S.FunctionTypeN _ t)) = typ c t
typ _ t = error $ show t

deriverRoot :: Config -> Type -> [I.Deriver]
deriverRoot config (S.FunctionType (S.FunctionTypeN ts _)) = deriverFunction config True ts
deriverRoot _ _                                            = []

deriver :: Config -> Type -> [I.Deriver]
deriver config (S.FunctionType (S.FunctionTypeN ts _)) = deriverFunction config False ts
deriver _ _                                            = []

deriverFunction :: Config -> Bool -> [Type] -> [I.Deriver]
deriverFunction config root ts =
  [ I.Pointer if root then [] else [I.Constant]
  , I.Function $ go <$> ts
  ]
  where
    go t = (typ config t, [I.Constant], Nothing, deriver config t)
