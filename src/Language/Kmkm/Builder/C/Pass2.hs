{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass2
  ( convert
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I

import           Data.Hashable         (Hashable (hash))
import qualified Data.List             as L
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Language.C            (CCompoundBlockItem (CBlockStmt), CConst, CConstant (CFloatConst, CIntConst),
                                        CDecl, CDeclaration (CDecl),
                                        CDeclarationSpecifier (CStorageSpec, CTypeQual, CTypeSpec),
                                        CDeclarator (CDeclr), CDerivedDeclarator (CFunDeclr), CEnumeration (CEnum),
                                        CExpr, CExpression (CCall, CCompoundLit, CConst, CVar), CExtDecl,
                                        CExternalDeclaration (CDeclExt, CFDefExt), CFloat (CFloat), CFunDef,
                                        CFunctionDef (CFunDef), CInit, CInitializer (CInitExpr, CInitList),
                                        CIntRepr (DecRepr, HexRepr, OctalRepr), CInteger (CInteger), CStat,
                                        CStatement (CCompound, CReturn), CStorageSpecifier (CTypedef),
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
element (I.Declaration (I.ValueDeclaration qs t i))       = CDeclExt $ valueDeclaration qs t i Nothing
element (I.Declaration (I.FunctionDeclaration qs t i ps)) = undefined qs t i ps
element (I.Definition (I.ValueDefinition qs t i l))       = CDeclExt $ valueDeclaration qs t (Just i) (Just l)
element (I.Definition (I.FunctionDefinition qs t i ps s)) = CFDefExt $ functionDefinition qs t i ps s
element (I.TypeDefinition t i)                            = CDeclExt $ typeDefinition t i

valueDeclaration :: [I.VariableQualifier] -> I.QualifiedType -> Maybe I.Identifier -> Maybe I.Initializer -> CDecl
valueDeclaration qs t i l =
  CDecl
    ((CTypeQual . variableQualifier <$> qs) ++ (CTypeSpec <$> qualifiedType t))
    [ ( Just $ CDeclr (identifier <$> i) [] Nothing [] undefNode
      , initializer <$> l
      , Nothing
      )
    ] undefNode

functionDefinition :: [I.VariableQualifier] -> I.QualifiedType -> I.Identifier -> [([I.VariableQualifier], I.QualifiedType, I.Identifier)] -> I.Statement -> CFunDef
functionDefinition qs t i ps s =
  CFunDef
    ((CTypeQual . variableQualifier <$> qs) ++ (CTypeSpec <$> qualifiedType t))
    (CDeclr (Just $ identifier i) [CFunDeclr (Right (parameter <$> ps, False)) [] undefNode] Nothing [] undefNode)
    []
    (statement s)
    undefNode
    where
      parameter (qs, t, i) =
        CDecl
          ((CTypeQual . variableQualifier <$> qs) ++ (CTypeSpec <$> qualifiedType t))
          [(Just $ CDeclr (Just $ identifier i) [] Nothing [] undefNode, Nothing, Nothing)]
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
typ I.Int = CIntType undefNode
typ I.Double = CDoubleType undefNode
typ (I.EnumerableLiteral i is) = enumerableLiteral i is
typ (I.StructureLiteral i fs) = structureLiteral i fs
typ (I.Union fs) = union fs
typ (I.TypeVariable "bool") = CBoolType undefNode
typ (I.TypeVariable i@(I.Identifier _)) = CTypeDef (identifier i) undefNode
typ (I.Enumerable i) = CEnumType (CEnum (Just $ identifier i) Nothing [] undefNode) undefNode
typ (I.Structure i) = CSUType (CStruct CStructTag (Just $ identifier i) Nothing [] undefNode) undefNode
typ t = error $ show t

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


initializer :: I.Initializer -> CInit
initializer (I.Expression e) = CInitExpr (expression e) undefNode
initializer (I.List is) =
  CInitList (go <$> is) undefNode
  where go i = ([], initializer i)

expression :: I.Expression -> CExpr
expression (I.Literal l)     = CConst $ literal l
expression (I.Variable v)    = CVar (identifier v) undefNode
expression (I.Compound t is) = CCompoundLit (CDecl (CTypeSpec <$> qualifiedType t) [] undefNode) (go <$> is) undefNode where go i = ([], initializer i)
expression (I.Call t as)     = CCall (expression t) (expression <$> as) undefNode
expression e                 = error $ show e

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
  CFloatConst (CFloat $ mconcat [hstr, istr, dstr, fstr, kstr, estr] ) undefNode
  where
    hstr, sstr, istr, fstr, dstr, kstr, estr :: String
    hstr = case b of { I.FractionDecimal -> ""; I.FractionHexadecimal -> "0x" }
    sstr = (case b of { I.FractionDecimal -> show; I.FractionHexadecimal -> flip showHex "" }) s
    (istr, fstr) = L.genericSplitAt (L.genericLength sstr - f) sstr
    dstr = if f == 0 then "" else "."
    kstr = case b of { I.FractionDecimal -> "e"; I.FractionHexadecimal -> "p" }
    estr = show e
literal l                = error $ show l

statement :: I.Statement -> CStat
statement (I.Return e) = CCompound [] [CBlockStmt $ CReturn (Just $ expression e) undefNode] undefNode
statement s            = error $ show s
