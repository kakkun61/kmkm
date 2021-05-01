{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass1
  ( convert
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I
import qualified Language.Kmkm.Syntax           as S
import           Language.Kmkm.Syntax.Base      (Identifier (SystemIdentifier, UserIdentifier), ModuleName (ModuleName))
import           Language.Kmkm.Syntax.Phase5    (Bind, Literal, Member, Module, Term, Type)
import qualified Language.Kmkm.Syntax.Type      as T
import qualified Language.Kmkm.Syntax.Value     as V

import           Data.Text (Text)
import qualified Data.Text as T

convert :: Module -> I.File
convert = module'

module' :: Module -> I.File
module' (S.Module n ms) = I.File (moduleName n) $ member =<< ms

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
member :: Member -> [I.Element]
member (S.Definition i cs) =
  mconcat
    [ tagEnum
    , [structure]
    , I.Definition . constructor (length cs) <$> cs
    ]
  where
    tagEnumIdent (UserIdentifier t)     = I.Identifier $ t <> "_tag"
    tagEnumIdent (SystemIdentifier t n) = I.Identifier $ T.pack $ '_' : t : show n ++ "_tag"
    tagEnumType = ([], I.Enumerable $ tagEnumIdent i)
    tagEnum =
      case cs of
        [(_, _:_)] -> []
        _ -> [I.Declaration $ I.ValueDeclaration ([], I.EnumerableLiteral (Just $ tagEnumIdent i) $ tagEnumIdent . fst <$> cs) [] Nothing]
    structType = ([], I.Structure $ identifier i)
    structure =
      I.Declaration $ I.ValueDeclaration ([], I.StructureLiteral (Just $ identifier i) fields) [] Nothing
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
        field (i, t) = I.Field (typ t) $ identifier i
        constructor (i, fs) = I.Field ([], I.StructureLiteral Nothing $ field <$> fs) $ identifier i
        hasFields = or $ go <$> cs where go (_, fs) = not $ null fs
    constructor _ (c, []) = I.ValueDefinition structType [I.Constant] (identifier c) $ I.List [I.Expression $ I.Variable $ tagEnumIdent c]
    constructor n (c, fs) =
      I.FunctionDefinition structType [] (identifier c) (parameter <$> fs) statement
      where
        parameter (i, t) = (typ t, [I.Constant], identifier i)
        statement = I.Return $ I.CompoundLiteral structType arguments
        arguments =
          case (n, fs) of
            (1, _:_) -> argument <$> fs
            _        -> I.Expression (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (i, _) = I.Expression $ I.Variable $ identifier i
member (S.Bind a) = [bind a]

bind :: Bind -> I.Element
bind (S.TermBind (S.TermBindV i v@(V.TypedTerm _ t))) = I.Definition $ I.ValueDefinition (typ t) [] (identifier i) $ I.Expression $ term v
bind (S.TermBind (S.TermBind0 i v@(V.TypedTerm _ t))) = I.Definition $ I.FunctionDefinition (typ t) [] (identifier i) [] $ I.Return $ term v
bind (S.TermBind (S.TermBind1 i i0 t0 v@(V.TypedTerm _ t))) = I.Definition $ I.FunctionDefinition (typ t) [] (identifier i) [(typ t0, [I.Constant], identifier i0)] $ I.Return $ term v
bind (S.TermBind (S.TermBind2 i i0 t0 i1 t1 v@(V.TypedTerm _ t))) = I.Definition $ I.FunctionDefinition (typ t) [] (identifier i) [(typ t0, [I.Constant], identifier i0), (typ t1, [I.Constant], identifier i1)] $ I.Return $ term v
bind (S.TermBind (S.TermBind3 i i0 t0 i1 t1 i2 t2 v@(V.TypedTerm _ t))) = I.Definition $ I.FunctionDefinition (typ t) [] (identifier i) [(typ t0, [I.Constant], identifier i0), (typ t1, [I.Constant], identifier i1), (typ t2, [I.Constant], identifier i2)] $ I.Return $ term v
bind a = error $ show a

identifier :: Identifier -> I.Identifier
identifier (UserIdentifier t)     = I.Identifier t
identifier (SystemIdentifier t n) = I.Identifier $ T.pack $ '_' : t : show n

moduleName :: ModuleName -> Text
moduleName (ModuleName n) = n

term :: Term -> I.Expression
term (V.TypedTerm (V.Variable i) _)                              = I.Variable $ identifier i
term (V.TypedTerm (V.Literal l) _)                               = I.Literal $ literal l
term (V.TypedTerm (V.Application (V.Application1 v v0)) _)       = I.Call (term v) [term v0]
term (V.TypedTerm (V.Application (V.Application2 v v0 t1)) _)    = I.Call (term v) $ term <$> [v0, t1]
term (V.TypedTerm (V.Application (V.Application3 v v0 t1 v2)) _) = I.Call (term v) $ term <$> [v0, t1, v2]

literal :: Literal -> I.Literal
literal (V.Integer i b) =
  I.Integer i $
    case b of
      2  -> I.IntBinary
      8  -> I.IntOctal
      10 -> I.IntDecimal
      16 -> I.IntHexadecimal
      _  -> error $ "literal: base: " ++ show b
literal (V.Fraction s f e b) =
  I.Fraction s f e $
    case b of
      10 -> I.FractionDecimal
      16 -> I.FractionHexadecimal
      _  -> error $ "literal: base " ++ show b
literal l = error (show l)

typ :: Type -> I.QualifiedType
typ (T.Variable "int")   = ([], I.Int)
typ (T.Variable "uint")  = ([I.Unsigned], I.Int)
typ (T.Variable "frac2") = ([], I.Double)
typ t                    = error $ show t
