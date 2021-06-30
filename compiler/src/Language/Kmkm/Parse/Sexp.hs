{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Language.Kmkm.Parse.Sexp
  ( parse
  , parse'
  , module'
  , definition
  , dataDefinition
  , valueBind
  , identifier
  , value
  , literal
  , integer
  , fraction
  , string
  , Parser
  , Exception (..)
  ) where

import qualified Language.Kmkm.Exception as X
import qualified Language.Kmkm.Syntax    as S

import           Control.Applicative        (Alternative (many, (<|>)))
import qualified Control.Exception          as E
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Data.Bool                  (bool)
import qualified Data.Char                  as C
import           Data.Functor               (($>))
import           Data.Functor.Identity      (Identity)
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as N
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Typeable              as Y
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import qualified Language.C.Data.Ident      as C
import qualified Language.C.Data.Position   as C
import qualified Language.C.Parser          as C
import qualified Language.C.Pretty          as C
import qualified Language.C.Syntax.AST      as C
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as M
import           Text.Megaparsec.Parsers    (ParsecT (ParsecT))
import qualified Text.Parser.Char           as P
import qualified Text.Parser.Combinators    as P
import qualified Text.Parser.Token          as P
import qualified Text.PrettyPrint           as R

type Module = S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Definition = S.Definition 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Type = S.Type 'S.NameUnresolved 'S.Curried

type FunctionType = S.FunctionType 'S.NameUnresolved 'S.Curried

type Value = S.Value 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Literal = S.Literal 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Function = S.Function 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Application = S.Application 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type TypeAnnotation = S.TypeAnnotation 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type ProcedureStep = S.ProcedureStep 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Parser = ParsecT Void Text Identity

parse :: MonadThrow m => String -> Text -> m Module
parse = parse' (module' <* P.eof)

parse' :: MonadThrow m => Parser a -> String -> Text -> m a
parse' (ParsecT p) n s =
  case M.parse p n s of
    Right a -> pure a
    Left e  -> throwM $ Exception $ M.errorBundlePretty e

parse'' :: MonadFail m => Parser a -> String -> Text -> m a
parse'' (ParsecT p) n s =
  case M.parse p n s of
    Right a -> pure a
    Left e  -> fail $ M.errorBundlePretty e

module' :: Parser Module
module' =
  M.label "module" $
    P.parens $ do
      void $ P.textSymbol "module"
      S.Module <$> moduleName <*> list moduleName <*> list definition

list :: Parser a -> Parser [a]
list p =
  M.label "list" $
    P.parens $ do
      void $ P.textSymbol "list"
      P.many p

list1 :: Parser a -> Parser (NonEmpty a)
list1 p =
  M.label "list1" $
    P.parens $ do
      void $ P.textSymbol "list"
      fromMaybe X.unreachable . N.nonEmpty <$> P.some p

definition :: Parser Definition
definition =
  M.label "definition" $
    P.parens $
      P.choice
        [ dataDefinition
        , foreignValueBind
        , valueBind
        , foreignTypeBind
        ]

dataDefinition :: Parser Definition
dataDefinition =
  M.label "dataDefinition" $ do
    void $ P.textSymbol "define"
    S.DataDefinition <$> identifier <*> list valueConstructor

valueConstructor :: Parser (S.Identifier, [(S.Identifier, Type)])
valueConstructor =
  M.label "valueConstructor" $
    P.choice
      [ (, []) <$> identifier
      , P.parens $ (,) <$> identifier <*> list field
      ]

field :: Parser (S.Identifier, Type)
field = M.label "field" $ P.parens $ (,) <$> identifier <*> typ

valueBind :: Parser Definition
valueBind =
  M.label "valueBind" $ do
    void $ P.textSymbol "bind-value"
    S.ValueBind <$> (S.ValueBindU <$> identifier <*> value)

foreignValueBind :: Parser Definition
foreignValueBind =
  M.label "foreignValueBind" $ do
    void $ P.textSymbol "bind-value-foreign"
    S.ForeignValueBind <$> identifier <*> list cHeader <*> (S.CDefinition <$> cExternalDeclaration) <*> typ

foreignTypeBind :: Parser Definition
foreignTypeBind =
  M.label "foreignTypeBind" $ do
    void $ P.textSymbol "bind-type-foreign"
    S.ForeignTypeBind <$> identifier <*> list cHeader <*> (S.CDefinition <$> (check =<< cExternalDeclaration))
  where
    check declExt@(C.CDeclExt (C.CDecl declSpecs [(Just (C.CDeclr (Just C.Ident {}) [] Nothing [] _), Nothing, Nothing)] _)) =
      case foldr declSpecAcc (pure (False, False)) declSpecs of
        Right (True, True) -> pure declExt
        Right (False, _)   -> fail "typedef must be used"
        Right (_, False)   -> fail "no typedef names are found"
        Left s             -> fail $ "\"" ++ R.render (C.pretty s) ++ "\" cannot be used"
    check declExt = fail $ "\"" ++ R.render (C.pretty declExt) ++ "\" cannot be used"
    declSpecAcc (C.CTypeSpec (C.CUnsigType _)) acc = acc >>= \(d, t) -> pure (d, t)
    declSpecAcc C.CTypeSpec {} acc                 = acc >>= \(d, _) -> pure (d, True)
    declSpecAcc (C.CStorageSpec C.CTypedef {}) acc = acc >>= \(_, ts) -> pure (True, ts)
    declSpecAcc s _                                = Left s

identifier :: Parser S.Identifier
identifier =
  M.label "identifier" $
    P.token $ do
      a <- asciiAlphabet
      b <- many $ P.choice [asciiAlphabet, P.digit]
      pure $ S.UserIdentifier $ T.pack $ a : b

eitherIdentifier :: Parser S.EitherIdentifier
eitherIdentifier =
  M.label "eitherIdentifier" $
    P.token $ do
      is <- dotSeparatedIdentifier
      let n = S.UserIdentifier $ N.last is
      case N.nonEmpty (N.init is) of
        Nothing -> pure $ S.UnqualifiedIdentifier n
        Just m  -> pure $ S.QualifiedIdentifier $ S.GlobalIdentifier (S.ModuleName m) n

moduleName :: Parser S.ModuleName
moduleName = M.label "moduleName" $ S.ModuleName <$> P.token dotSeparatedIdentifier

dotSeparatedIdentifier :: Parser (N.NonEmpty Text)
dotSeparatedIdentifier =
  M.label "dotSeparatedIdentifier" $ P.sepByNonEmpty identifierSegment (P.char '.')

identifierSegment :: Parser Text
identifierSegment = do
  a <- asciiAlphabet
  b <- many $ P.choice [asciiAlphabet, P.digit]
  pure $ T.pack $ a : b

value :: Parser Value
value =
  M.label "value'" $
    S.UntypedValue <$>
      P.choice
        [ S.Variable <$> eitherIdentifier
        , S.Literal <$> literal
        , P.parens $
            P.choice
              [ S.Literal . S.Function <$> function
              , S.Application <$> application
              , S.Procedure <$> procedure
              , S.TypeAnnotation <$> typeAnnotation
              , S.Let <$> (P.textSymbol "let" *> list definition) <*> value
              ]
        ]

literal :: Parser Literal
literal =
  M.label "literal" $
    P.choice
      [ P.try fraction
      , integer
      , S.String <$> string
      ]

application :: Parser Application
application =
  M.label "application" $ do
    void $ P.textSymbol "apply"
    S.ApplicationC <$> value <*> value

procedure :: Parser (NonEmpty ProcedureStep)
procedure =
  M.label "procedure" $ do
    void $ P.textSymbol "procedure"
    list1 p
  where
    p =
      M.label "procedure.p" $
        P.parens $
          P.choice
            [ do
                void $ P.textSymbol "bind"
                S.BindProcedure <$> identifier <*> value
            , do
                void $ P.textSymbol "term"
                S.TermProcedure <$> value
            ]

typeAnnotation :: Parser TypeAnnotation
typeAnnotation =
  M.label "typeAnnotation" $ do
    void $ P.textSymbol "type"
    S.TypeAnnotation' <$> value <*> typ

integer :: Parser Literal
integer =
  M.label "integer" $ do
    P.token $
      P.choice
        [ P.text "0b" >> flip S.Integer 2 <$> M.binary
        , P.text "0o" >> flip S.Integer 8 <$> M.octal
        , P.text "0x" >> flip S.Integer 16 <$> M.hexadecimal
        , flip S.Integer 10 <$> M.decimal
        ]

fraction :: Parser Literal
fraction =
  M.label "fraction" $ do P.token $ hexadecimal <|> decimal
  where
    hexadecimal = do
      void $ P.text "0x"
      istr <- digits 16
      fstr <- P.option "" $ P.text "." >> digits 16
      let
        fractionDigits :: Word
        fractionDigits = fromIntegral $ T.length fstr
      significand <- parse'' M.hexadecimal "fraction.hexadecimal.significand" $ istr <> fstr
      void $ P.text "p"
      epos <- sign
      estr <- digits 16
      exponent <- if T.null estr then pure 0 else parse'' M.decimal "fraction.hexadecimal.exponent" estr
      pure $ S.Fraction significand fractionDigits (sign' epos exponent) 16
    decimal = do
      istr <- digits 10
      (fstr, epos, estr) <-
        P.choice
          [ do
              void $ P.text "."
              fstr <- digits 10
              (epos, estr) <-
                P.option (True, "0") $ do
                  void $ P.text "e"
                  (,) <$> sign <*> digits 10
              pure (fstr, epos, estr)
          , do
              void $ P.text "e"
              (,,) ""  <$> sign <*> digits 10
          ]
      let
        fractionDigits :: Word
        fractionDigits = fromIntegral $ T.length fstr
      significand <- parse'' M.decimal "fraction.decimal.significand" $ istr <> fstr
      exponent <- if T.null estr then pure 0 else parse'' M.decimal "fraction.decimal.exponent" estr
      pure $ S.Fraction significand fractionDigits (sign' epos exponent) 10
    sign' :: Num n => Bool -> n -> n
    sign' = bool negate id

digits :: Word -> Parser Text
digits b = do
  let bs = L.genericTake b ['0' .. '9'] ++ ((++) <$> id <*> (C.toUpper <$>) $ L.genericTake (b - 10) ['a' ..])
  T.pack <$> many (P.choice $ P.char <$> bs)

sign :: Parser Bool
sign =  P.option True $ P.text "-" $> False <|> P.text "+" $> True

string :: Parser Text
string =
  M.label "string" $ do
    P.token $
      P.choice
        [ do
            void $ P.text "\"\"\""
            let
              go n = do
                c <- P.anyChar
                case c of
                  '"' ->
                    if n == 2
                      then pure T.empty
                      else go (n + 1)
                  _ ->
                    if n == 0
                      then (T.singleton c <>) <$> go n
                      else ((T.replicate n "\"" <> T.singleton c) <>) <$> go 0
            go 0
        , doubleQuote $
            fmap mconcat $
              many $
                P.choice
                  [ P.text "\\\"" $> "\""
                  , T.singleton <$> P.notChar '"'
                  ]
        ]

function :: Parser Function
function =
  M.label "function" $ do
    void $ P.textSymbol "function"
    S.FunctionC <$> identifier <*> typ <*> value

typ :: Parser Type
typ =
  M.label "type" $ do
    P.choice
      [ S.TypeVariable <$> eitherIdentifier
      , P.parens $
          P.choice
            [ S.FunctionType <$> functionType
            , uncurry S.TypeApplication <$> typeApplication
            , S.ProcedureType <$> procedureStep
            ]
      ]

functionType :: Parser FunctionType
functionType =
  M.label "functionType" $ do
    void $ P.textSymbol "function"
    S.FunctionTypeC <$> typ <*> typ

typeApplication :: Parser (Type, Type)
typeApplication =
  M.label "typeApplication" $ do
    void $ P.textSymbol "apply"
    (,) <$> typ <*> typ

procedureStep :: Parser Type
procedureStep =
  M.label "procedureStep" $ do
    void $ P.textSymbol "procedure"
    typ

cExternalDeclaration :: Parser C.CExtDecl
cExternalDeclaration = do
  s <- T.encodeUtf8 <$> string
  case C.execParser_ C.extDeclP s C.nopos of
    Left (C.ParseError (m, _)) -> fail $ unlines m
    Right c                    -> pure c

cHeader :: Parser S.CHeader
cHeader =
  P.parens $
    P.choice
      [ do
          void $ P.textSymbol "system-header"
          S.SystemHeader <$> string
      , do
          void $ P.textSymbol "local-header"
          S.LocalHeader <$> string
      ]

doubleQuote :: Parser a -> Parser a
doubleQuote = M.label "doubleQuote" . (`P.surroundedBy` P.text "\"")

asciiAlphabet :: Parser Char
asciiAlphabet = P.choice [asciiUpper, asciiLower]

asciiUpper :: Parser Char
asciiUpper = P.choice $ P.char <$> ['A' .. 'Z']

asciiLower :: Parser Char
asciiLower = P.choice $ P.char <$> ['a' .. 'z']

newtype Exception
  = Exception String
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
