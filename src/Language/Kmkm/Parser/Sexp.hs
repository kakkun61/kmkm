{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Parser.Sexp
  ( parse
  , parse'
  , module'
  , definition
  , alias
  , identifier
  , term
  , literal
  , integer
  , string
  , bool
  ) where

import           Language.Kmkm.Syntax       (Alias (Term, Type), Member (Alias, Definition), Module (Module))
import           Language.Kmkm.Syntax.Base  (Identifier (Identifier))
import           Language.Kmkm.Syntax.Type  (Type)
import qualified Language.Kmkm.Syntax.Type  as T
import           Language.Kmkm.Syntax.Value (Literal (Bool, Fraction, String), Term (Literal))
import qualified Language.Kmkm.Syntax.Value as V

import           Control.Applicative        (Alternative (many))
import           Control.Monad              (void)
import           Data.Functor               (($>))
import           Data.Functor.Identity      (Identity)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as M
import           Text.Megaparsec.Parsers    (ParsecT (ParsecT))
import qualified Text.Parser.Char           as C
import           Text.Parser.Combinators    ((<?>))
import qualified Text.Parser.Combinators    as C
import qualified Text.Parser.Token          as C

type Parser = ParsecT Void Text Identity

parse :: MonadFail m => String -> Text -> m Module
parse = parse' (module' <* C.eof)

parse' :: MonadFail m => Parser a -> String -> Text -> m a
parse' (ParsecT p) n s =
  case M.parse p n s of
    Right a -> pure a
    Left e  -> fail $ M.errorBundlePretty e

module' :: Parser Module
module' =
  (<?> "module") $
    C.parens $ do
      void $ C.textSymbol "module"
      i <- C.token identifier
      Module i <$> list member

list :: Parser a -> Parser [a]
list p =
  (<?> "list") $
    C.parens $ do
      void $ C.textSymbol "list"
      C.many p

member :: Parser Member
member =
  "member" <!>
    C.choice
      [ C.try definition
      , Alias <$> alias
      ]

definition :: Parser Member
definition =
  (<?> "definition") $
    C.parens $ do
      void $ C.textSymbol "define"
      i <- C.token identifier
      Definition i <$> list valueConstructor

valueConstructor :: Parser (Identifier, [(Identifier, Type)])
valueConstructor =
  "valueConstructor" <!>
    C.choice
      [ do
          i <- C.token identifier
          pure (i, [])
      , C.parens $ do
          i <- C.token identifier
          fs <- list field
          pure (i, fs)
      ]

field :: Parser (Identifier, Type)
field = (<?> "field") $ C.parens $ (,) <$> C.token identifier <*> typ

alias :: Parser Alias
alias =
  (<?> "alias") $
    C.parens $ do
      void $ C.textSymbol "alias"
      i <- C.token identifier
      C.choice
        [ Term i <$> term <*> typ
        , Type i <$> undefined
        ]

identifier :: Parser Identifier
identifier =
  "identifier" <!> do
    a <- asciiAlphabet
    b <- many $ C.choice [asciiAlphabet, C.digit, C.char '_']
    pure $ Identifier $ T.pack $ a:b

term :: Parser Term
term =
  "term" <!>
    C.choice
      [ V.Variable <$> C.token identifier
      , Literal <$> literal
      , C.parens $ V.Application <$> term <*> term
      ]

literal :: Parser Literal
literal =
  "literal" <!>
    C.choice
      [ (\s -> Fraction s 0 0) <$> integer
      , String <$> string
      , Bool <$> bool
      ]

integer :: Parser Integer
integer =
  "integer" <!> do
    C.choice $
      C.token <$>
        [ C.text "0b" >> M.binary
        , C.text "0x" >> M.hexadecimal
        , C.text "0o" >> M.octal
        , C.decimal
        ]

string :: Parser Text
string =
  "string" <!> do
    mconcat <$> do
      C.token $
        doubleQuotes $
          many $
            C.choice
              [ C.text "\\\"" >> pure "\""
              , T.singleton <$> C.notChar '"'
              ]

bool :: Parser Bool
bool =
  "bool" <!> do
    C.choice $
      C.token <$>
        [ C.text "false" $> False
        , C.text "true" $> True
        ]

typ :: Parser Type
typ =
  "type" <!> do
    C.choice
      [ T.Variable <$> C.token identifier
      , C.parens $ T.Application <$> typ <*> typ
      ]

doubleQuotes :: Parser a -> Parser a
doubleQuotes = (<?> "doubleQuotes") . C.between (void $ C.text "\"") (void $ C.text "\"")

asciiAlphabet :: Parser Char
asciiAlphabet = C.choice [asciiUpper, asciiLower]

asciiUpper :: Parser Char
asciiUpper = C.choice $ C.char <$> ['A' .. 'Z']

asciiLower :: Parser Char
asciiLower = C.choice $ C.char <$> ['a' .. 'z']

(<!>) :: String -> Parser a -> Parser a
(<!>) = flip (<?>)
infix <!>
