{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Builder.C.Pass2
  ( convert
  , bind
  ) where

import qualified Language.Kmkm.Builder.C.Syntax as I
import           Language.Kmkm.Config           (Config (Config, typeMap))
import qualified Language.Kmkm.Config           as C
import qualified Language.Kmkm.Exception        as X
import qualified Language.Kmkm.Syntax           as S
import           Language.Kmkm.Syntax.Base      (Identifier (SystemIdentifier, UserIdentifier), ModuleName (ModuleName))
import           Language.Kmkm.Syntax.Phase6    (Bind, Literal, Member, Module, ProcedureStep, Term, Type)
import qualified Language.Kmkm.Syntax.Type      as T
import qualified Language.Kmkm.Syntax.Value     as V

import qualified Data.List.NonEmpty as N
import           Data.Text          (Text)
import qualified Data.Text          as T

convert :: Config -> Module -> I.File
convert = module'

module' :: Config -> Module -> I.File
module' config (S.Module n ms) = I.File (moduleName n) $ member config =<< ms

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
member :: Config -> Member -> [I.Element]
member config (S.Definition i cs) =
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
        field (i, t) = I.Field (typ config t) $ identifier i
        constructor (i, fs) = I.Field ([], I.StructureLiteral Nothing $ field <$> fs) $ identifier i
        hasFields = or $ go <$> cs where go (_, fs) = not $ null fs
    constructor _ (c, []) = I.ExpressionDefinition structType [I.Constant] (identifier c) [] $ I.List [I.Expression $ I.Variable $ tagEnumIdent c]
    constructor n (c, fs) =
      I.StatementDefinition structType [] (identifier c) [I.Function $ parameter <$> fs] [blockItem]
      where
        parameter (i, t) = (typ config t, [I.Constant], Just $ identifier i, [])
        blockItem = I.BlockStatement $ I.Return $ I.CompoundLiteral structType arguments
        arguments =
          case (n, fs) of
            (1, _:_) -> argument <$> fs
            _        -> I.Expression (I.Variable $ tagEnumIdent c) : (argument <$> fs)
        argument (i, _) = I.Expression $ I.Variable $ identifier i
member config (S.Bind a) = [bind config a]

bind :: Config -> Bind -> I.Element
bind config (S.TermBind (S.TermBindV i v@(V.TypedTerm _ t)) _)  = I.Definition $ I.ExpressionDefinition (typ config t) [] (identifier i) (deriver config t) $ I.Expression $ term config v
bind config (S.TermBind (S.TermBind0 i v) ms)                   = bindTermN config i [] v ms
bind config (S.TermBind (S.TermBind1 i i0 t0 v) ms)             = bindTermN config i [(i0, t0)] v ms
bind config (S.TermBind (S.TermBind2 i i0 t0 i1 t1 v) ms)       = bindTermN config i [(i0, t0), (i1, t1)] v ms
bind config (S.TermBind (S.TermBind3 i i0 t0 i1 t1 i2 t2 v) ms) = bindTermN config i [(i0, t0), (i1, t1), (i2, t2)] v ms
bind config (S.TypeBind i t)                                    = I.TypeDefinition (typ config t) $ identifier i

bindTermN :: Config -> Identifier -> [(Identifier, Type)] -> Term -> [Member] -> I.Element
bindTermN config i ps v@(V.TypedTerm _ t) ms =
  I.Definition $ I.StatementDefinition (typ config t) [] (identifier i) (I.Function (parameter <$> ps) : deriverRoot config t) $ (elementStatement <$> (member config =<< ms)) ++ [I.BlockStatement $ I.Return (term config v)]
  where
    parameter (i, t) = (typ config t, case t of { T.Arrow {} -> []; _ -> [I.Constant] }, Just $ identifier i, deriver config t)

elementStatement :: I.Element -> I.BlockElement
elementStatement (I.Declaration t qs i ds) = I.BlockDeclaration t qs i ds
elementStatement (I.Definition d)          = I.BlockDefinition d
elementStatement (I.TypeDefinition t i)    = I.BlockTypeDefinition t i

identifier :: Identifier -> I.Identifier
identifier (UserIdentifier t)     = I.Identifier t
identifier (SystemIdentifier t n) = I.Identifier $ T.pack $ '_' : t : show n

moduleName :: ModuleName -> Text
moduleName (ModuleName n) = n

term :: Config -> Term -> I.Expression
term _ (V.TypedTerm (V.Variable i) _)                                   = I.Variable $ identifier i
term _ (V.TypedTerm (V.Literal l) _)                                    = I.Literal $ literal l
term config (V.TypedTerm (V.Application (V.Application0 v)) _)          = I.Call (term config v) []
term config (V.TypedTerm (V.Application (V.Application1 v v0)) _)       = I.Call (term config v) [term config v0]
term config (V.TypedTerm (V.Application (V.Application2 v v0 t1)) _)    = I.Call (term config v) $ term config <$> [v0, t1]
term config (V.TypedTerm (V.Application (V.Application3 v v0 t1 v2)) _) = I.Call (term config v) $ term config <$> [v0, t1, v2]
term config (V.TypedTerm (V.Procedure ps) _)                            = I.StatementExpression $ I.Block $ procedureStep config =<< N.toList ps
term _ (V.TypedTerm (V.TypeAnnotation _) _)                             = X.unreachable

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

procedureStep :: Config -> ProcedureStep -> [I.BlockElement ]
procedureStep config (V.BindProcedure i v@(V.TypedTerm _ t)) = [I.BlockDeclaration (typ config t) [I.Constant] (Just $ identifier i) [], I.BlockStatement $ I.ExpressionStatement $ I.Assign (identifier i) (term config v)]
procedureStep config (V.TermProcedure v) = [I.BlockStatement $ I.ExpressionStatement $ term config v]

typ :: Config -> Type -> I.QualifiedType
typ Config { typeMap } (T.Variable n) =
  get typeMap
  where
    get =
      case n of
        "int"   -> I.readCType . C.int
        "uint"  -> I.readCType . C.uint
        "byte"  -> I.readCType . C.byte
        "frac"  -> I.readCType . C.frac
        "frac2" -> I.readCType . C.frac2
        _       -> const ([], I.Structure $ identifier n)
typ c (T.Arrow (T.Arrow1 _ t)) = typ c t
typ _ t = error $ show t

deriverRoot :: Config -> Type -> [I.Deriver]
deriverRoot config (T.Arrow (T.Arrow1 t1 _))       = deriverArrow config True [t1]
deriverRoot config (T.Arrow (T.Arrow2 t1 t2 _))    = deriverArrow config True [t1, t2]
deriverRoot config (T.Arrow (T.Arrow3 t1 t2 t3 _)) = deriverArrow config True [t1, t2, t3]
deriverRoot _ _                                    = []

deriver :: Config -> Type -> [I.Deriver]
deriver config (T.Arrow (T.Arrow1 t1 _))       = deriverArrow config False [t1]
deriver config (T.Arrow (T.Arrow2 t1 t2 _))    = deriverArrow config False [t1, t2]
deriver config (T.Arrow (T.Arrow3 t1 t2 t3 _)) = deriverArrow config False [t1, t2, t3]
deriver _ _                                    = []

deriverArrow :: Config -> Bool -> [Type] -> [I.Deriver]
deriverArrow config root ts = [I.Pointer if root then [] else [I.Constant], I.Function $ go <$> ts] where go t = (typ config t, [I.Constant], Nothing, deriver config t)
