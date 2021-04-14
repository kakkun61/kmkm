{-# LANGUAGE CPP               #-}
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
  , fraction
  , string
  ) where

import           Language.Kmkm.Syntax       (Alias (Term, Type), Member (Alias, Definition), Module (Module))
import           Language.Kmkm.Syntax.Base  (Identifier (Identifier))
import           Language.Kmkm.Syntax.Type  (Type)
import qualified Language.Kmkm.Syntax.Type  as T
import           Language.Kmkm.Syntax.Value (Literal (Fraction, Integer, String), Term (Literal))
import qualified Language.Kmkm.Syntax.Value as V

import           Control.Applicative        (Alternative (many, (<|>)))
import           Control.Monad              (void)
import           Data.Bool                  (bool)
import qualified Data.Char                  as C
import           Data.Functor               (($>))
import           Data.Functor.Identity      (Identity)
import qualified Data.List                  as L
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as M
import           Text.Megaparsec.Parsers    (ParsecT (ParsecT))
import qualified Text.Parser.Char           as P
import           Text.Parser.Combinators    ((<?>))
import qualified Text.Parser.Combinators    as P
import qualified Text.Parser.Token          as P

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif


type Parser = ParsecT Void Text Identity

parse :: MonadFail m => String -> Text -> m Module
parse = parse' (module' <* P.eof)

parse' :: MonadFail m => Parser a -> String -> Text -> m a
parse' (ParsecT p) n s =
  case M.parse p n s of
    Right a -> pure a
    Left e  -> fail $ M.errorBundlePretty e

module' :: Parser Module
module' =
  (<?> "module") $
    P.parens $ do
      void $ P.textSymbol "module"
      i <- P.token identifier
      Module i <$> list member

list :: Parser a -> Parser [a]
list p =
  (<?> "list") $
    P.parens $ do
      void $ P.textSymbol "list"
      P.many p

member :: Parser Member
member =
  "member" <!>
    P.choice
      [ P.try definition
      , Alias <$> alias
      ]

definition :: Parser Member
definition =
  (<?> "definition") $
    P.parens $ do
      void $ P.textSymbol "define"
      i <- P.token identifier
      Definition i <$> list valueConstructor

valueConstructor :: Parser (Identifier, [(Identifier, Type)])
valueConstructor =
  "valueConstructor" <!>
    P.choice
      [ do
          i <- P.token identifier
          pure (i, [])
      , P.parens $ do
          i <- P.token identifier
          fs <- list field
          pure (i, fs)
      ]

field :: Parser (Identifier, Type)
field = (<?> "field") $ P.parens $ (,) <$> P.token identifier <*> typ

alias :: Parser Alias
alias =
  (<?> "alias") $
    P.parens $ do
      void $ P.textSymbol "alias"
      i <- P.token identifier
      P.choice
        [ Term i <$> term <*> typ
        , Type i <$> undefined
        ]

identifier :: Parser Identifier
identifier =
  "identifier" <!> do
    a <- asciiAlphabet
    b <- many $ P.choice [asciiAlphabet, P.digit, P.char '_']
    pure $ Identifier $ T.pack $ a:b

term :: Parser Term
term =
  "term" <!>
    P.choice
      [ V.Variable <$> P.token identifier
      , Literal <$> literal
      , P.parens $ V.Application <$> term <*> term
      ]

literal :: Parser Literal
literal =
  "literal" <!>
    P.choice
      [ P.try fraction
      , integer
      , String <$> string
      ]

integer :: Parser Literal
integer =
  "integer" <!> do
    P.token $
      P.choice
        [ P.text "0b" >> flip Integer 2 <$> M.binary
        , P.text "0o" >> flip Integer 8 <$> M.octal
        , P.text "0x" >> flip Integer 16 <$> M.hexadecimal
        , flip Integer 10 <$> M.decimal
        ]

fraction :: Parser Literal
fraction =
  "fraction" <!> do P.token $ hexadecimal <|> decimal
  where
    hexadecimal, decimal :: Parser Literal
    hexadecimal = do
      void $ P.text "0x"
      istr <- digits 16
      fstr <- P.option "" $ P.text "." >> digits 16
      let
        fractionDigits :: Word
        fractionDigits = fromIntegral $ T.length fstr
      significand <- parse' M.hexadecimal "fraction.hexadecimal.significand" $ istr <> fstr
      void $ P.text "p"
      epos <- sign
      estr <- digits 16
      exponent <- if T.null estr then pure 0 else parse' M.decimal "fraction.hexadecimal.exponent" estr
      pure $ Fraction significand fractionDigits (sign' epos exponent) 16
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
                  epos <- sign
                  (,) epos <$> digits 10
              pure (fstr, epos, estr)
          , do
              void $ P.text "e"
              epos <- sign
              (,,) "" epos <$> digits 10
          ]
      let
        fractionDigits :: Word
        fractionDigits = fromIntegral $ T.length fstr
      significand <- parse' M.decimal "fraction.decimal.significand" $ istr <> fstr
      exponent <- if T.null estr then pure 0 else parse' M.decimal "fraction.decimal.exponent" estr
      pure $ Fraction significand fractionDigits (sign' epos exponent) 10
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
    mconcat <$> do
      P.token $
        doubleQuotes $
          many $
            P.choice
              [ P.text "\\\"" >> pure "\""
              , T.singleton <$> P.notChar '"'
              ]

typ :: Parser Type
typ =
  "type" <!> do
    P.choice
      [ T.Variable <$> P.token identifier
      , P.parens $ T.Application <$> typ <*> typ
      ]

doubleQuotes :: Parser a -> Parser a
doubleQuotes = (<?> "doubleQuotes") . P.between (void $ P.text "\"") (void $ P.text "\"")

asciiAlphabet :: Parser Char
asciiAlphabet = P.choice [asciiUpper, asciiLower]

asciiUpper :: Parser Char
asciiUpper = P.choice $ P.char <$> ['A' .. 'Z']

asciiLower :: Parser Char
asciiLower = P.choice $ P.char <$> ['a' .. 'z']

(<!>) :: String -> Parser a -> Parser a
(<!>) = flip (<?>)
infix <!>
