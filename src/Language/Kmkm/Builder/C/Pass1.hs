{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass1
  ( convert
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I
import qualified Language.Kmkm.Syntax           as S
import           Language.Kmkm.Syntax.Base      (Identifier (Identifier))
import           Language.Kmkm.Syntax.Phase2    (Application (Application), Bind, Function (Function), Literal, Member,
                                                 Module, Term, Type)
import qualified Language.Kmkm.Syntax.Type      as T
import qualified Language.Kmkm.Syntax.Value     as V

import Data.Coerce (Coercible, coerce)

convert :: Module -> I.File
convert = module'

module' :: Module -> I.File
module' (S.Module (Identifier t) ms) =
  I.File t $ member =<< ms

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
    tagEnumIdent (Identifier t) = I.Identifier $ t <> "_tag"
    tagEnumType = ([], I.Enumerable $ tagEnumIdent i)
    tagEnum =
      case cs of
        [(_, _:_)] -> []
        _ -> [I.Declaration $ I.ValueDeclaration [] ([], I.EnumerableLiteral (Just $ tagEnumIdent i) $ tagEnumIdent . fst <$> cs) Nothing]
    structType = ([], I.Structure $ identifier i)
    structure =
      I.Declaration $ I.ValueDeclaration [] ([], I.StructureLiteral (Just $ identifier i) fields) Nothing
      where
        fields =
          case cs of
            [(_, fs@(_:_))] -> field <$> fs
            _ ->
              I.Field tagEnumType (I.Identifier "tag")
                :
                  if hasFields
                    then [I.Field ([], I.Union $ constructor <$> cs) (I.Identifier "body")]
                    else []
        field (i, t) = I.Field (typ t) $ identifier i
        constructor (i, fs) = I.Field ([], I.StructureLiteral Nothing $ field <$> fs) $ identifier i
        hasFields = or $ go <$> cs where go (_, fs) = not $ null fs
    constructor _ (c, []) = I.ValueDefinition [I.Constant] structType (identifier c) $ I.List [I.Expression $ I.Variable $ tagEnumIdent c]
    constructor n (c, fs) =
      I.FunctionDefinition [] structType (identifier c) (parameter <$> fs) statement
      where
        parameter (i, t) = ([I.Constant], typ t, identifier i)
        statement = I.Return $ I.Compound structType arguments
        arguments =
          case (n, fs) of
            (1, _:_) -> argument <$> fs
            _        -> I.Expression (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (i, _) = I.Expression $ I.Variable $ identifier i
member (S.Bind a) = [bind a]

bind :: Bind -> I.Element
bind (S.Term i (V.Literal (V.Function' f)) (T.Arrow' a)) = I.Definition $ functionDefinition i f a
bind (S.Term i v t) = I.Definition $ I.ValueDefinition [] (typ t) (identifier i) $ I.Expression $ term v
bind a            = error $ show a

functionDefinition :: Identifier -> Function -> T.ArrowN -> I.Definition
functionDefinition i (Function (V.Function1 i0 v)) (T.Arrow1 t0 t) =
  I.FunctionDefinition [] (typ t) (identifier i) [([I.Constant], typ t0, identifier i0)] $ I.Return $ term v
functionDefinition i (Function (V.Function2 i0 i1 v)) (T.Arrow2 t0 t1 t) =
  I.FunctionDefinition [] (typ t) (identifier i) [([I.Constant], typ t0, identifier i0), ([I.Constant], typ t1, identifier i1)] $ I.Return $ term v
functionDefinition i (Function (V.Function3 i0 i1 i2 v)) (T.Arrow3 t0 t1 t2 t) =
  I.FunctionDefinition [] (typ t) (identifier i) [([I.Constant], typ t0, identifier i0), ([I.Constant], typ t1, identifier i1), ([I.Constant], typ t2, identifier i2)] $ I.Return $ term v
functionDefinition i f a = error $ show (i, f, a)

identifier :: Identifier -> I.Identifier
identifier (Identifier t) = I.Identifier t

term :: Coercible a Term => a -> I.Expression
term = term' . coerce

term' :: Term -> I.Expression
term' (V.Variable i)                                             = I.Variable $ identifier i
term' (V.Literal l)                                              = I.Literal $ literal l
term' (V.Application' (Application (V.Application1 t t0)))       = I.Call (term t) [term t0]
term' (V.Application' (Application (V.Application2 t t0 t1)))    = I.Call (term t) $ term <$> [t0, t1]
term' (V.Application' (Application (V.Application3 t t0 t1 t2))) = I.Call (term t) $ term <$> [t0, t1, t2]

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
typ (T.Variable (Identifier "int"))   = ([], I.Int)
typ (T.Variable (Identifier "uint"))  = ([I.Unsigned], I.Int)
typ (T.Variable (Identifier "frac2")) = ([], I.Double)
typ t                                 = error $ show t
