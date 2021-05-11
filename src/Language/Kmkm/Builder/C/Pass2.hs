{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass2
  ( convert
  , bind
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I
import qualified Language.Kmkm.Syntax           as S
import           Language.Kmkm.Syntax.Base      (Identifier (SystemIdentifier, UserIdentifier), ModuleName (ModuleName))
import           Language.Kmkm.Syntax.Phase6    (Bind, Literal, Member, Module, Term, Type)
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
        _ -> [I.Declaration ([], I.EnumerableLiteral (Just $ tagEnumIdent i) $ tagEnumIdent . fst <$> cs) [] Nothing []]
    structType = ([], I.Structure $ identifier i)
    structure =
      I.Declaration ([], I.StructureLiteral (Just $ identifier i) fields) [] Nothing []
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
    constructor _ (c, []) = I.ExpressionDefinition structType [I.Constant] (identifier c) [] $ I.List [I.Expression $ I.Variable $ tagEnumIdent c]
    constructor n (c, fs) =
      I.StatementDefinition structType [] (identifier c) [I.Function $ parameter <$> fs] [statement]
      where
        parameter (i, t) = (typ t, [I.Constant], Just $ identifier i, [])
        statement = I.Return $ I.CompoundLiteral structType arguments
        arguments =
          case (n, fs) of
            (1, _:_) -> argument <$> fs
            _        -> I.Expression (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (i, _) = I.Expression $ I.Variable $ identifier i
member (S.Bind a) = [bind a]

bind :: Bind -> I.Element
bind (S.TermBind (S.TermBindV i v@(V.TypedTerm _ t)) _)  = I.Definition $ I.ExpressionDefinition (typ t) [] (identifier i) (deriver t) $ I.Expression $ term v
bind (S.TermBind (S.TermBind0 i v) ms)                   = bindTermN i [] v ms
bind (S.TermBind (S.TermBind1 i i0 t0 v) ms)             = bindTermN i [(i0, t0)] v ms
bind (S.TermBind (S.TermBind2 i i0 t0 i1 t1 v) ms)       = bindTermN i [(i0, t0), (i1, t1)] v ms
bind (S.TermBind (S.TermBind3 i i0 t0 i1 t1 i2 t2 v) ms) = bindTermN i [(i0, t0), (i1, t1), (i2, t2)] v ms
bind a                                                   = error $ show a

bindTermN :: Identifier -> [(Identifier, Type)] -> Term -> [Member] -> I.Element
bindTermN i ps v@(V.TypedTerm _ t) ms =
  I.Definition $ I.StatementDefinition (typ t) [] (identifier i) (I.Function (parameter <$> ps) : deriverRoot t) $ (elementStatement <$> (member =<< ms)) ++ [I.Return (term v)]
  where
    parameter (i, t) = (typ t, case t of { T.Arrow {} -> []; _ -> [I.Constant] }, Just $ identifier i, deriver t)

elementStatement :: I.Element -> I.Statement
elementStatement (I.Declaration t qs i ds) = I.DeclarationStatement t qs i ds
elementStatement (I.Definition d)          = I.DefinitionStatement d
elementStatement (I.TypeDefinition t i)    = I.TypeDefinitionStatement t i

identifier :: Identifier -> I.Identifier
identifier (UserIdentifier t)     = I.Identifier t
identifier (SystemIdentifier t n) = I.Identifier $ T.pack $ '_' : t : show n

moduleName :: ModuleName -> Text
moduleName (ModuleName n) = n

term :: Term -> I.Expression
term (V.TypedTerm (V.Variable i) _)                              = I.Variable $ identifier i
term (V.TypedTerm (V.Literal l) _)                               = I.Literal $ literal l
term (V.TypedTerm (V.Application (V.Application0 v)) _)          = I.Call (term v) []
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
typ (T.Variable "int")       = ([], I.Int)
typ (T.Variable "uint")      = ([I.Unsigned], I.Int)
typ (T.Variable "frac2")     = ([], I.Double)
typ (T.Arrow (T.Arrow1 _ t)) = typ t
typ t                        = error $ show t

deriverRoot :: Type -> [I.Deriver]
deriverRoot (T.Arrow (T.Arrow1 t1 _))       = deriverArrow True [t1]
deriverRoot (T.Arrow (T.Arrow2 t1 t2 _))    = deriverArrow True [t1, t2]
deriverRoot (T.Arrow (T.Arrow3 t1 t2 t3 _)) = deriverArrow True [t1, t2, t3]
deriverRoot _                               = []

deriver :: Type -> [I.Deriver]
deriver (T.Arrow (T.Arrow1 t1 _))       = deriverArrow False [t1]
deriver (T.Arrow (T.Arrow2 t1 t2 _))    = deriverArrow False [t1, t2]
deriver (T.Arrow (T.Arrow3 t1 t2 t3 _)) = deriverArrow False [t1, t2, t3]
deriver _                               = []

deriverArrow :: Bool -> [Type] -> [I.Deriver]
deriverArrow root ts = [I.Pointer if root then [] else [I.Constant], I.Function $ go <$> ts] where go t = (typ t, [I.Constant], Nothing, deriver t)
