{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Phase2
  ( file
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I
import Language.C
import qualified Data.Text as Text
import Language.C.Data.Ident
import Data.Hashable (Hashable(hash))
import Debug.Trace
import Data.Text (Text)

file :: I.File -> CTranslUnit
file (I.File _ es) = flip CTranslUnit undefNode $ element <$> es

element :: I.Element -> CExtDecl
element (I.Declaration (I.ValueDeclaration qs t i)) = CDeclExt $ valueDeclaration qs t i Nothing
element (I.Declaration (I.FunctionDeclaration qs t i ps)) = undefined
element (I.Definition (I.ValueDefinition qs t i l)) = CDeclExt $ valueDeclaration qs t (Just i) (Just l)
element (I.Definition (I.FunctionDefinition qs t i ps s)) = CFDefExt $ functionDefinition qs t i ps s
element (I.TypeDefinition t i) = CDeclExt $ typeDefinition t i

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
textIdentifier t = Ident (Text.unpack t) (hash t) undefNode

variableQualifier :: I.VariableQualifier -> CTypeQual
variableQualifier I.Constant = CConstQual undefNode

qualifiedType :: I.QualifiedType -> [CTypeSpec]
qualifiedType (qs, t) = (typeQualifier <$> qs) ++ [typ t]

typeQualifier :: I.TypeQualifier -> CTypeSpec
typeQualifier I.Unsigned = CUnsigType undefNode

typ :: I.Type -> CTypeSpec
typ I.Int = CIntType undefNode
typ (I.EnumerableLiteral i is) = enumerableLiteral i is
typ (I.StructureLiteral i fs) = structureLiteral i fs
typ (I.Union fs) = union fs
typ (I.TypeVariable (I.Identifier "bool")) = CBoolType undefNode
typ (I.TypeVariable i@(I.Identifier _)) = CTypeDef (identifier i) undefNode
typ (I.Enumerable i) = CEnumType (CEnum (Just $ identifier i) Nothing [] undefNode) undefNode
typ (I.Structure i) = CSUType (CStruct CStructTag (Just $ identifier i) Nothing [] undefNode) undefNode
typ t = trace (show t) undefined

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
expression (I.Literal l) = CConst $ literal l
expression (I.Variable v) = CVar (identifier v) undefNode
expression (I.Compound t is) = CCompoundLit (CDecl (CTypeSpec <$> qualifiedType t) [] undefNode) (go <$> is) undefNode where go i = ([], initializer i)
expression e = trace (show e) undefined

literal :: I.Literal -> CConst
literal (I.Integer i) = CIntConst (CInteger i DecRepr noFlags) undefNode
literal l = trace (show l) undefined

statement :: I.Statement -> CStat
statement (I.Return e) = CCompound [] [CBlockStmt $ CReturn (Just $ expression e) undefNode] undefNode
statement s = trace (show s) undefined
