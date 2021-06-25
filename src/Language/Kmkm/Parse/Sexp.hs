{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Language.Kmkm.Parse.Sexp
  ( parse
  , parse'
  , module'
  , definition
  , dataDefinition
  , valueBind
  , identifier
  , term
  , literal
  , integer
  , fraction
  , string
  , Parser
  , Exception (..)
  ) where

import qualified Language.Kmkm.Exception     as X
import           Language.Kmkm.Syntax        (CDefinition (CDefinition), CHeader (LocalHeader, SystemHeader),
                                              Identifier (UserIdentifier), ModuleName (ModuleName),
                                              QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax        as S
-- import           Language.Kmkm.Syntax.Phase1 (Application, Definition, Function, FunctionType, Literal, Module,
--                                               ProcedureStep, Term, Type, TypeAnnotation)

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
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Typeable              as Y
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import qualified Language.C.Data.Position   as C
import qualified Language.C.Parser          as C
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as M
import           Text.Megaparsec.Parsers    (ParsecT (ParsecT))
import qualified Text.Parser.Char           as P
import qualified Text.Parser.Combinators    as P
import qualified Text.Parser.Token          as P

type Module = S.Module 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Definition = S.Definition 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Type = S.Type 'S.Curried

type FunctionType = S.FunctionType 'S.Curried

type Term = S.Term 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Literal = S.Literal 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Function = S.Function 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type Application = S.Application 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type TypeAnnotation = S.TypeAnnotation 'S.Curried 'S.LambdaUnlifted 'S.Untyped

type ProcedureStep = S.ProcedureStep 'S.Curried 'S.LambdaUnlifted 'S.Untyped

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
      N.fromList <$> P.some p -- never empty list.

definition :: Parser Definition
definition =
  M.label "definition" $
    P.parens $
      P.choice
        [ dataDefinition
        , foreignValueBind
        , valueBind
        ]

dataDefinition :: Parser Definition
dataDefinition =
  M.label "dataDefinition" $ do
    void $ P.textSymbol "define"
    S.DataDefinition <$> identifier <*> list valueConstructor

valueConstructor :: Parser (Identifier, [(Identifier, Type)])
valueConstructor =
  M.label "valueConstructor" $
    P.choice
      [ flip (,) [] <$> identifier
      , P.parens $ (,) <$> identifier <*> list field
      ]

field :: Parser (Identifier, Type)
field = M.label "field" $ P.parens $ (,) <$> identifier <*> typ

valueBind :: Parser Definition
valueBind =
  M.label "valueBind" $ do
    void $ P.textSymbol "bind-value"
    S.ValueBind <$> (S.BindU <$> identifier <*> term)

foreignValueBind :: Parser Definition
foreignValueBind =
  M.label "foreignValueBind" $ do
    void $ P.textSymbol "bind-value-foreign"
    S.ForeignValueBind <$> identifier <*> list cHeader <*> cDefinition <*> typ

identifier :: Parser Identifier
identifier =
  M.label "identifier" $
    P.token $ do
      a <- asciiAlphabet
      b <- many $ P.choice [asciiAlphabet, P.digit]
      pure $ UserIdentifier $ T.pack $ a : b

qualifiedIdentifier :: Parser QualifiedIdentifier
qualifiedIdentifier =
  M.label "qualifiedIdentifier" $
    P.token $ do
      is <- dotSeparatedIdentifier
      let
        m = ModuleName <$> N.nonEmpty (N.init is)
        n = UserIdentifier $ N.last is
      pure $ QualifiedIdentifier m n

moduleName :: Parser ModuleName
moduleName = M.label "moduleName" $ ModuleName <$> P.token dotSeparatedIdentifier

dotSeparatedIdentifier :: Parser (N.NonEmpty Text)
dotSeparatedIdentifier =
  M.label "dotSeparatedIdentifier" $ P.sepByNonEmpty identifierSegment (P.char '.')

identifierSegment :: Parser Text
identifierSegment = do
  a <- asciiAlphabet
  b <- many $ P.choice [asciiAlphabet, P.digit]
  pure $ T.pack $ a : b

term :: Parser Term
term =
  M.label "term'" $
    S.UntypedTerm <$>
      P.choice
        [ S.Variable <$> qualifiedIdentifier
        , S.Literal <$> literal
        , P.parens $
            P.choice
              [ S.Literal . S.Function <$> function
              , S.Application <$> application
              , S.Procedure <$> procedure
              , S.TypeAnnotation <$> typeAnnotation
              , S.Let <$> (P.textSymbol "let" *> list definition) <*> term
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
    S.ApplicationC <$> term <*> term

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
                S.BindProcedure <$> identifier <*> term
            , do
                void $ P.textSymbol "term"
                S.TermProcedure <$> term
            ]

typeAnnotation :: Parser TypeAnnotation
typeAnnotation =
  M.label "typeAnnotation" $ do
    void $ P.textSymbol "type"
    S.TypeAnnotation' <$> term <*> typ

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
    S.FunctionC <$> identifier <*> typ <*> term

typ :: Parser Type
typ =
  M.label "type" $ do
    P.choice
      [ S.TypeVariable <$> identifier
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

cDefinition :: Parser CDefinition
cDefinition = do
  s <- T.encodeUtf8 <$> string
  case C.execParser_ C.extDeclP s C.nopos of
    Left (C.ParseError (m, _)) -> fail $ unlines m
    Right c                    -> pure $ CDefinition c

cHeader :: Parser CHeader
cHeader =
  P.parens $
    P.choice
      [ do
          void $ P.textSymbol "system-header"
          SystemHeader <$> string
      , do
          void $ P.textSymbol "local-header"
          LocalHeader <$> string
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
