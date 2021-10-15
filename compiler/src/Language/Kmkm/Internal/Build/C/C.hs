{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Build.C.C
  ( render
  ) where

import Language.Kmkm.Internal.Build.C.Syntax (ArithmeticExpression,
                                              BlockElement (BlockDeclaration, BlockDefinition, BlockStatement, BlockTypeDefinition),
                                              Definition (ExpressionDefinition, StatementDefinition),
                                              Deriver (Function, Pointer),
                                              Element (Declaration, Definition, TypeDefinition),
                                              Expression (ArithmeticExpression, Assign, Call, CompoundLiteral, Literal, StatementExpression, Variable),
                                              Field (Field), File (File),
                                              FractionBase (FractionDecimal, FractionHexadecimal),
                                              Identifier (Identifier),
                                              Initializer (ExpressionInitializer, ListInitializer),
                                              IntBase (IntBinary, IntDecimal, IntHexadecimal, IntOctal),
                                              Literal (Fraction, Integer, String), QualifiedType,
                                              Statement (Block, Case, ExpressionStatement, If, Return),
                                              Type (Char, Double, Enumerable, EnumerableLiteral, Float, Int, Structure, StructureLiteral, TypeVariable, Union, Void),
                                              TypeQualifier (Unsigned), VariableQualifier (Constant, External))

import           Data.Foldable (Foldable (fold))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Numeric       (showHex, showOct)
import qualified Data.Char as C

render :: File -> Text
render (File _ es) =
  T.intercalate "\n\n" $ text element <$> es

element :: Element -> Text
element (Declaration t qs i ds) = variable t qs (Right <$> i) ds <> ";"
element (Definition d)          = definition d
element (TypeDefinition t i)    = typeDefinition t i

definition :: Definition -> Text
definition (ExpressionDefinition t qs i ds n) =
  T.unwords
    [ variable t qs (Just $ Right i) ds
    , "="
    , initializer n
     ]
  <> ";"
definition (StatementDefinition t qs i ds es) =
  T.intercalate "\n" $
    [ variable t qs (Just $ Right i) ds
    , "{"
    ]
    <> either pure (text blockElement <$>) es
    <> ["}"]

qualifiedType :: QualifiedType -> Text
qualifiedType (qs, t) =
  T.unwords $
    (typeQualifier <$> qs)
    <> [typ t]

typeDefinition :: Either Text QualifiedType -> Identifier -> Text
typeDefinition t i = "typedef " <> text qualifiedType t <> " " <> identifier i <> ";"

typ :: Type -> Text
typ Void = "void"
typ Char = "char"
typ Int = "int"
typ Double = "double"
typ Float = "float"
typ (TypeVariable i) = identifier i
typ (Structure i) = "struct " <> identifier i
typ (Enumerable i) = "enum " <> identifier i
typ (StructureLiteral i fs) =
  T.intercalate "\n"
    [ "struct" <> maybe "" ((" " <>) . identifier) i <> " {"
    , T.intercalate "\n" ((<> ";") . field <$> fs)
    , "}"
    ]
typ (Union fs) =
  T.intercalate "\n"
    [ "union {"
    , T.intercalate "\n" ((<> ";") . field <$> fs)
    , "}"
    ]
typ (EnumerableLiteral i is) =
  T.intercalate "\n"
    [ "enum" <> maybe "" ((" " <>) . identifier) i <> " {"
    , T.intercalate ",\n" (identifier <$> is)
    , "}"
    ]

field :: Field -> Text
field (Field t i) = qualifiedType t <> " " <> identifier i

typeQualifier :: TypeQualifier -> Text
typeQualifier Unsigned = "unsigned"

variableQualifier :: VariableQualifier -> Text
variableQualifier Constant = "const"
variableQualifier External = "extern"

identifier :: Identifier -> Text
identifier (Identifier t) = t

variable :: QualifiedType -> [VariableQualifier] -> Maybe (Either Text Identifier) -> [Deriver] -> Text
variable t qs i ds =
  T.unwords $
    [qualifiedType t]
    <> (variableQualifier <$> qs)
    <> case foldl deriver (maybe "" (text identifier) i) ds of { "" -> []; t -> [t] }

deriver :: Text -> Deriver -> Text
deriver t (Pointer qs) =
  fold
    [ T.unwords $ ["(*"] <> (variableQualifier <$> qs) <> [t]
    , ")"
    ]
deriver t (Function ps) =
  fold
    [ t
    , "("
    , T.intercalate ", " (uncurry4 variable <$> ps)
    , ")"
    ]

initializer :: Initializer -> Text
initializer (ExpressionInitializer e) = text expression e
initializer (ListInitializer es) =
  fold
    [ "{ "
    , T.intercalate ", " $ initializer <$> es
    , " }"
    ]

expression :: Expression -> Text
expression (Variable i)             = identifier i
expression (Literal l)              = literal l
expression (CompoundLiteral t ns)   = compoundLiteral t ns
expression (ArithmeticExpression a) = arithmeticExpression a
expression (Call e es)              = expression e <> "(" <> T.intercalate ", " (expression <$> es) <> ")"
expression (StatementExpression es) =
  T.intercalate "\n"
    [ "({"
    , T.intercalate "\n" $ text blockElement <$> es
    , "})"
    ]
expression (Assign i e)             = identifier i <> " = " <> expression e

literal :: Literal -> Text
literal (Integer v IntBinary) = "0x" <> T.pack (showHex v "")
literal (Integer v IntOctal) = "0" <> T.pack (showOct v "")
literal (Integer v IntDecimal) = T.pack $ show v
literal (Integer v IntHexadecimal) = "0x" <> T.pack (showHex v "")
literal (Fraction s f e b) =
  fold [hstr, istr, dstr, fstr, kstr, estr]
  where
    hstr, sstr, istr, fstr, dstr, kstr, estr :: Text
    hstr = case b of { FractionDecimal -> ""; FractionHexadecimal -> "0x" }
    sstr = T.pack $ (case b of { FractionDecimal -> show; FractionHexadecimal -> flip showHex "" }) s
    (istr, fstr) = T.splitAt (T.length sstr - fromIntegral f) sstr
    dstr = if f == 0 then "" else "."
    kstr = case b of { FractionDecimal -> "e"; FractionHexadecimal -> "p" }
    estr = T.pack $ show e
literal (String t) = "u8\"" <> escapeText t <> "\""

escapeText :: Text -> Text
escapeText s
  | T.null s = s
  | c <- T.head s
  , s' <- T.tail s =
    if C.isAscii c
      then
        case c of
          '\n' -> "\\n" <> escapeText s'
          '\t' -> "\\t" <> escapeText s'
          '\r' -> "\\r" <> escapeText s'
          '\v' -> "\\v" <> escapeText s'
          '\b' -> "\\b" <> escapeText s'
          '\f' -> "\\f" <> escapeText s'
          '\a' -> "\\a" <> escapeText s'
          '\'' -> "\\'" <> escapeText s'
          '\"' -> "\\\"" <> escapeText s'
          '\\' -> "\\\\" <> escapeText s'
          _ -> T.cons c $escapeText s'
      else "\\U" <> pad 8 '0' (T.pack (showHex (C.ord c) "")) <> escapeText s'

pad :: Word -> Char -> Text -> Text
pad n c s =
  go (fromIntegral n - T.length s) s
  where
    go n s
      | n > 0 = go (n - 1) (T.cons c s)
      | otherwise = s

compoundLiteral :: QualifiedType -> [Initializer] -> Text
compoundLiteral t ns =
  fold
    [ "("
    , qualifiedType t
    , ") { "
    , T.intercalate ", " $ initializer <$> ns
    , " }"
    ]

arithmeticExpression :: ArithmeticExpression -> Text
arithmeticExpression = undefined

blockElement :: BlockElement -> Text
blockElement (BlockStatement s)           = statement s
blockElement (BlockDeclaration t qs i ds) = variable t qs (Right <$> i) ds
blockElement (BlockDefinition d)          = definition d
blockElement (BlockTypeDefinition t i)    = typeDefinition t i

statement :: Statement -> Text
statement (ExpressionStatement e) = expression e <> ";"
statement (Return e) = "return " <> expression e <> ";"
statement (If e ts es) =
  T.intercalate "\n"
    [ "if (" <> expression e <> ")"
    , "{"
    , T.intercalate "\n" $ statement ts : maybe [] (pure . statement) es
    , "}"
    ]
statement (Case _e _bs) = undefined
statement (Block es) =
  T.intercalate "\n"
    [ "{"
    , T.intercalate "\n" $ text blockElement <$> es
    , "}"
    ]

text :: (a -> Text) -> Either Text a -> Text
text = either id

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 fun (a, b, c, d) = fun a b c d
