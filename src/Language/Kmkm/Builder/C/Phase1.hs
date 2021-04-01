{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Phase1
  ( module'
  , member
  ) where

import           Language.Kmkm.Syntax       (Alias (Term), Member (Alias, Definition), Module (Module))
import           Language.Kmkm.Syntax.Base  (Identifier (Identifier))
import           Language.Kmkm.Syntax.Type  (Type)
import qualified Language.Kmkm.Syntax.Type  as T
import           Language.Kmkm.Syntax.Value (Literal (Fraction), Term (Literal))
import qualified Language.Kmkm.Builder.C.Syntax as I

module' :: Module -> I.File
module' (Module (Identifier t) ms) =
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
member (Definition i cs) =
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
            _ -> I.Expression (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (i, _) = I.Expression $ I.Variable $ identifier i
member (Alias a) = [alias a]

alias :: Alias -> I.Element
alias (Term i v t) = I.Definition $ I.ValueDefinition [] (typ t) (identifier i) $ I.Expression $ term v

identifier :: Identifier -> I.Identifier
identifier (Identifier t) = I.Identifier t

term :: Term -> I.Expression
term (Literal l) = I.Literal $ literal l

literal :: Literal -> I.Literal
literal (Fraction s e b) | 0 <= e = fraction s e b

fraction :: Integer -> Integer -> Integer -> I.Literal
fraction s e _ | e == 0 = I.Integer s

typ :: Type -> I.QualifiedType
typ (T.Variable (Identifier "int")) = ([], I.Int)
typ (T.Variable (Identifier "uint")) = ([I.Unsigned], I.Int)
