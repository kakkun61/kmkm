{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass4
  ( convert
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I

import           Data.Hashable         (Hashable (hash))
import qualified Data.List             as L
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Language.C            (CBlockItem, CCompoundBlockItem (CBlockDecl, CBlockStmt, CNestedFunDef), CConst,
                                        CConstant (CFloatConst, CIntConst), CDecl, CDeclaration (CDecl),
                                        CDeclarationSpecifier (CStorageSpec, CTypeQual, CTypeSpec),
                                        CDeclarator (CDeclr), CDerivedDeclarator (CFunDeclr, CPtrDeclr), CDerivedDeclr,
                                        CEnumeration (CEnum), CExpr,
                                        CExpression (CCall, CCompoundLit, CConst, CStatExpr, CVar), CExtDecl,
                                        CExternalDeclaration (CDeclExt, CFDefExt), CFloat (CFloat), CFunDef,
                                        CFunctionDef (CFunDef), CInit, CInitializer (CInitExpr, CInitList),
                                        CIntRepr (DecRepr, HexRepr, OctalRepr), CInteger (CInteger), CStat,
                                        CStatement (CCompound, CExpr, CReturn), CStorageSpecifier (CTypedef),
                                        CStructTag (CStructTag, CUnionTag), CStructureUnion (CStruct), CTranslUnit,
                                        CTranslationUnit (CTranslUnit), CTypeQual, CTypeQualifier (CConstQual),
                                        CTypeSpec,
                                        CTypeSpecifier (CBoolType, CDoubleType, CEnumType, CIntType, CSUType, CTypeDef, CUnsigType),
                                        Ident, noFlags, undefNode)
import           Language.C.Data.Ident (Ident (Ident))
import           Numeric               (showHex)

convert :: I.File -> CTranslUnit
convert = file

file :: I.File -> CTranslUnit
file (I.File _ es) = flip CTranslUnit undefNode $ element <$> es

element :: I.Element -> CExtDecl
element (I.Declaration t qs i ds)                           = CDeclExt $ valueDeclaration t qs i ds Nothing
element (I.Definition (I.ExpressionDefinition t qs i ds l)) = CDeclExt $ valueDeclaration t qs (Just i) ds (Just l)
element (I.Definition (I.StatementDefinition t qs i ds is)) = CFDefExt $ functionDefinition t qs i ds is
element (I.TypeDefinition t i)                              = CDeclExt $ typeDefinition t i
element (I.Embed (I.C c))                                   = c

valueDeclaration :: I.QualifiedType -> [I.VariableQualifier] -> Maybe I.Identifier -> [I.Deriver] -> Maybe I.Initializer -> CDecl
valueDeclaration t qs i ds l =
  CDecl
    ((CTypeSpec <$> qualifiedType t) ++ (CTypeQual . variableQualifier <$> qs))
    [ ( Just $ CDeclr (identifier <$> i) (deriver <$> ds) Nothing [] undefNode
      , initializer <$> l
      , Nothing
      )
    ] undefNode

functionDefinition :: I.QualifiedType -> [I.VariableQualifier] -> I.Identifier -> [I.Deriver] -> [I.BlockElement] -> CFunDef
functionDefinition t qs i ds is =
  CFunDef
    ((CTypeSpec <$> qualifiedType t) ++ (CTypeQual . variableQualifier <$> qs))
    (CDeclr (Just $ identifier i) (deriver <$> ds) Nothing [] undefNode)
    []
    (CCompound [] (blockItem <$> is) undefNode)
    undefNode

typeDefinition :: I.QualifiedType -> I.Identifier -> CDecl
typeDefinition t i =
  CDecl
    (CStorageSpec (CTypedef undefNode) : (CTypeSpec <$> qualifiedType t))
    [ ( Just $ CDeclr (Just $ identifier i) [] Nothing [] undefNode
      , Nothing
      , Nothing
      )
    ]
    undefNode

identifier :: I.Identifier -> Ident
identifier (I.Identifier t) = textIdentifier t

textIdentifier :: Text -> Ident
textIdentifier t = Ident (T.unpack t) (hash t) undefNode

variableQualifier :: I.VariableQualifier -> CTypeQual
variableQualifier I.Constant = CConstQual undefNode

qualifiedType :: I.QualifiedType -> [CTypeSpec]
qualifiedType (qs, t) = (typeQualifier <$> qs) ++ [typ t]

typeQualifier :: I.TypeQualifier -> CTypeSpec
typeQualifier I.Unsigned = CUnsigType undefNode

typ :: I.Type -> CTypeSpec
typ I.Int                               = CIntType undefNode
typ I.Double                            = CDoubleType undefNode
typ (I.EnumerableLiteral i is)          = enumerableLiteral i is
typ (I.StructureLiteral i fs)           = structureLiteral i fs
typ (I.Union fs)                        = union fs
typ (I.TypeVariable "bool")             = CBoolType undefNode
typ (I.TypeVariable i@(I.Identifier _)) = CTypeDef (identifier i) undefNode
typ (I.Enumerable i)                    = CEnumType (CEnum (Just $ identifier i) Nothing [] undefNode) undefNode
typ (I.Structure i)                     = CSUType (CStruct CStructTag (Just $ identifier i) Nothing [] undefNode) undefNode
typ t                                   = error $ show t

enumerableLiteral :: Maybe I.Identifier -> [I.Identifier] -> CTypeSpec
enumerableLiteral i is =
  CEnumType
    (CEnum
      (identifier <$> i)
      (Just $ value <$> is)
      []
      undefNode
    )
    undefNode
  where
    value i = (identifier i, Nothing)

structureLiteral :: Maybe I.Identifier -> [I.Field] -> CTypeSpec
structureLiteral = structureUnionLiteral CStructTag

union :: [I.Field] -> CTypeSpec
union = structureUnionLiteral CUnionTag Nothing

structureUnionLiteral :: CStructTag -> Maybe I.Identifier -> [I.Field] -> CTypeSpec
structureUnionLiteral t i fs =
  CSUType (CStruct t (identifier <$> i) fields [] undefNode) undefNode
    where
      fields =
        Just
          case fs of
            (_:_) -> field <$> fs
            _ ->
              [CDecl
                [CTypeSpec $ CEnumType (CEnum (identifier <$> i) Nothing [] undefNode) undefNode]
                [(Just $ CDeclr (Just $ textIdentifier "tag") [] Nothing [] undefNode, Nothing, Nothing)]
                undefNode
              ]
      field (I.Field t i) =
        CDecl
          (CTypeSpec <$> qualifiedType t)
          [(Just $ CDeclr (Just $ identifier i) [] Nothing [] undefNode, Nothing, Nothing)]
          undefNode

deriver :: I.Deriver -> CDerivedDeclr
deriver (I.Pointer qs) = CPtrDeclr (variableQualifier <$> qs) undefNode
deriver (I.Function ps) =
  CFunDeclr (Right (parameter <$> ps, False)) [] undefNode
  where
    parameter (t, qs, i, ds) =
      CDecl
        ((CTypeSpec <$> qualifiedType t) ++ (CTypeQual . variableQualifier <$> qs))
        [(Just $ CDeclr (identifier <$> i) (deriver <$> ds) Nothing [] undefNode, Nothing, Nothing)]
        undefNode

initializer :: I.Initializer -> CInit
initializer (I.Expression e) = CInitExpr (expression e) undefNode
initializer (I.List is) =
  CInitList (go <$> is) undefNode
  where go i = ([], initializer i)

expression :: I.Expression -> CExpr
expression (I.Literal l)             = CConst $ literal l
expression (I.Variable v)            = CVar (identifier v) undefNode
expression (I.CompoundLiteral t is)  = CCompoundLit (CDecl (CTypeSpec <$> qualifiedType t) [] undefNode) (go <$> is) undefNode where go i = ([], initializer i)
expression (I.Call t as)             = CCall (expression t) (expression <$> as) undefNode
expression (I.StatementExpression s) = CStatExpr (statement s) undefNode
expression e                         = error $ show e

literal :: I.Literal -> CConst
literal (I.Integer i b) =
  CIntConst (CInteger i repr noFlags) undefNode
  where
    repr =
      case b of
        I.IntBinary      -> HexRepr
        I.IntOctal       -> OctalRepr
        I.IntDecimal     -> DecRepr
        I.IntHexadecimal -> HexRepr
literal (I.Fraction s f e b) =
  CFloatConst (CFloat $ mconcat [hstr, istr, dstr, fstr, kstr, estr]) undefNode
  where
    hstr, sstr, istr, fstr, dstr, kstr, estr :: String
    hstr = case b of { I.FractionDecimal -> ""; I.FractionHexadecimal -> "0x" }
    sstr = (case b of { I.FractionDecimal -> show; I.FractionHexadecimal -> flip showHex "" }) s
    (istr, fstr) = L.genericSplitAt (L.genericLength sstr - f) sstr
    dstr = if f == 0 then "" else "."
    kstr = case b of { I.FractionDecimal -> "e"; I.FractionHexadecimal -> "p" }
    estr = show e
literal l = error $ show l

blockItem :: I.BlockElement -> CBlockItem
blockItem (I.BlockStatement s)                                     = CBlockStmt $ statement s
blockItem (I.BlockDefinition (I.ExpressionDefinition t qs i ds l)) = CBlockDecl $ valueDeclaration t qs (Just i) ds (Just l)
blockItem (I.BlockDefinition (I.StatementDefinition t qs i ds s))  = CNestedFunDef $ functionDefinition t qs i ds s
blockItem e                                                        = error $ show e

statement :: I.Statement -> CStat
statement (I.Return e)              = CReturn (Just $ expression e) undefNode
statement (I.Block is)              = CCompound [] (blockItem <$> is) undefNode
statement (I.ExpressionStatement e) = CExpr (Just $ expression e) undefNode
statement s                         = error $ show s
