{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Parser.Sexp
  ( parse
  , parse'
  , module'
  , member
  , definition
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
import qualified Language.Kmkm.Syntax        as S
import           Language.Kmkm.Syntax.Base   (Identifier (UserIdentifier), ModuleName (ModuleName),
                                              QualifiedIdentifier (QualifiedIdentifier))
import           Language.Kmkm.Syntax.Phase1 (Application, Function, Literal, Member, Module, ProcedureStep, TFunction,
                                              Term, Type, TypeAnnotation)
import qualified Language.Kmkm.Syntax.Type   as T
import qualified Language.Kmkm.Syntax.Value  as V

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
import           Text.Parser.Combinators    ((<?>))
import qualified Text.Parser.Combinators    as P
import qualified Text.Parser.Token          as P

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
      S.Module <$> moduleName <*> list moduleName <*> list member

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

member :: Parser Member
member =
  "member" <!>
    P.choice
      [ P.try definition
      , P.try valueBind
      , P.try foreignValueBind
      ]

definition :: Parser Member
definition =
  M.label "definition" $
    P.parens $ do
      void $ P.textSymbol "define"
      S.Definition <$> identifier <*> list valueConstructor

valueConstructor :: Parser (Identifier, [(Identifier, Type)])
valueConstructor =
  "valueConstructor" <!>
    P.choice
      [ flip (,) [] <$> identifier
      , P.parens $ (,) <$> identifier <*> list field
      ]

field :: Parser (Identifier, Type)
field = M.label "field" $ P.parens $ (,) <$> identifier <*> typ

valueBind :: Parser Member
valueBind =
  M.label "valueBind" $
    P.parens $ do
      void $ P.textSymbol "bind-value"
      S.ValueBind <$> (S.ValueBindU <$> identifier <*> term) <*> list member

foreignValueBind :: Parser Member
foreignValueBind =
  M.label "foreignValueBind" $
    P.parens $ do
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
    V.UntypedTerm <$>
      P.choice
        [ V.Variable <$> qualifiedIdentifier
        , P.try $ V.Literal <$> literal
        , P.try $ V.Application <$> application
        , P.try $ V.Procedure <$> procedure
        , V.TypeAnnotation <$> typeAnnotation
        ]

literal :: Parser Literal
literal =
  "literal" <!>
    P.choice
      [ P.try fraction
      , integer
      , V.String <$> string
      , V.Function <$> function
      ]

application :: Parser Application
application =
  M.label "application" $
    P.parens $ do
      void $ P.textSymbol "apply"
      V.ApplicationC <$> term <*> term

procedure :: Parser (NonEmpty ProcedureStep)
procedure =
  M.label "procedure" $
    P.parens $ do
      void $ P.textSymbol "procedure"
      list1 p
  where
    p =
      M.label "procedures.p" $
        P.parens $
          P.choice
            [ do
                void $ P.textSymbol "bind"
                V.BindProcedure <$> identifier <*> term
            , do
                void $ P.textSymbol "term"
                V.TermProcedure <$> term
            ]

typeAnnotation :: Parser TypeAnnotation
typeAnnotation =
  M.label "typeAnnotation" $
    P.parens $ do
      void $ P.textSymbol "type"
      V.TypeAnnotation' <$> term <*> typ

integer :: Parser Literal
integer =
  "integer" <!> do
    P.token $
      P.choice
        [ P.text "0b" >> flip V.Integer 2 <$> M.binary
        , P.text "0o" >> flip V.Integer 8 <$> M.octal
        , P.text "0x" >> flip V.Integer 16 <$> M.hexadecimal
        , flip V.Integer 10 <$> M.decimal
        ]

fraction :: Parser Literal
fraction =
  "fraction" <!> do P.token $ hexadecimal <|> decimal
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
      pure $ V.Fraction significand fractionDigits (sign' epos exponent) 16
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
      pure $ V.Fraction significand fractionDigits (sign' epos exponent) 10
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
  "string" <!> do
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
  M.label "function" $
    P.parens $ do
      void $ P.textSymbol "function"
      V.FunctionC <$> identifier <*> typ <*> term

typ :: Parser Type
typ =
  "type" <!> do
    P.choice
      [ T.Variable <$> identifier
      , P.try $ T.Function <$> arrow
      , P.try $ uncurry T.Application <$> typeApplication
      , T.Procedure <$> procedureStep
      ]

arrow :: Parser TFunction
arrow =
  M.label "arrow" $
    P.parens $ do
      void $ P.textSymbol "function"
      T.FunctionC <$> typ <*> typ

typeApplication :: Parser (Type, Type)
typeApplication =
  M.label "typeApplication" $
    P.parens $ do
      void $ P.textSymbol "apply"
      (,) <$> typ <*> typ

procedureStep :: Parser Type
procedureStep =
  M.label "procedureStep" $
    P.parens $ do
      void $ P.textSymbol "procedure"
      typ

cDefinition :: Parser S.CDefinition
cDefinition = do
  s <- T.encodeUtf8 <$> string
  case C.execParser_ C.extDeclP s C.nopos of
    Left (C.ParseError (m, _)) -> fail $ unlines m
    Right c                    -> pure $ S.CDefinition c

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

(<!>) :: String -> Parser a -> Parser a
(<!>) = flip (<?>)
infix <!>

newtype Exception
  = Exception String
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
