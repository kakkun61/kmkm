{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Kmkm.Internal.Parse.Sexp
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
  , withPosition
  , list
  , list1
  , Parser
  , EmbeddedParser (..)
  , EmbeddedValue
  , EmbeddedType
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Exception as X
import qualified Language.Kmkm.Internal.Syntax    as S

import qualified Barbies.Bare               as B
import           Control.Applicative        (Alternative (many, (<|>)))
import qualified Control.Exception          as E
import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad              (void)
import           Control.Monad.Reader       (MonadReader (ask), Reader, runReader)
import           Data.Bool                  (bool)
import qualified Data.Char                  as C
import           Data.Copointed             (Copointed (copoint))
import           Data.Functor               (($>))
import qualified Data.Kind                  as K
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as N
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Typeable              as Y
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as M
import           Text.Megaparsec.Parsers    (ParsecT (ParsecT))
import qualified Text.Parser.Char           as P
import qualified Text.Parser.Combinators    as P
import qualified Text.Parser.Token          as P

type Module et ev = S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type Definition et ev = S.Definition 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type Type = S.Type 'S.NameUnresolved 'S.Curried B.Covered S.WithLocation

type FunctionType = S.FunctionType 'S.NameUnresolved 'S.Curried B.Covered S.WithLocation

type Value et ev = S.Value 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type Function et ev = S.Function 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type Application et ev = S.Application 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type TypeAnnotation et ev = S.TypeAnnotation 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type ProcedureStep et ev  = S.ProcedureStep 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered S.WithLocation

type EmbeddedValue = S.EmbeddedValue B.Covered S.WithLocation

type EmbeddedType = S.EmbeddedType B.Covered S.WithLocation

type Parser et ev = ParsecT Void Text (Reader (Env et ev))

type Env :: (K.Type -> (K.Type -> K.Type) -> K.Type) -> (K.Type -> (K.Type -> K.Type) -> K.Type) -> K.Type
data Env et ev =
  Env
    { filePath        :: FilePath
    , embeddedParsers :: [EmbeddedParser et ev]
    , isEmbeddedType  :: S.EmbeddedType B.Covered S.WithLocation -> Maybe (et B.Covered S.WithLocation)
    , isEmbeddedValue :: S.EmbeddedValue B.Covered S.WithLocation -> Maybe (ev B.Covered S.WithLocation)
    }

parse
  :: MonadThrow m
  => [EmbeddedParser et ev] -- ^ Embedded parser.
  -> (S.EmbeddedType B.Covered S.WithLocation -> Maybe (et B.Covered S.WithLocation)) -- ^ Embedded type filter.
  -> (S.EmbeddedValue B.Covered S.WithLocation -> Maybe (ev B.Covered S.WithLocation)) -- ^ Embedded value filter.
  -> String -- ^ File name.
  -> Text -- ^ Input.
  -> m (S.WithLocation (Module et ev))
parse eps fv ft = parse' eps fv ft $ module' <* P.eof

parse'
  :: MonadThrow m
  => [EmbeddedParser et ev]
  -> (S.EmbeddedType B.Covered S.WithLocation -> Maybe (et B.Covered S.WithLocation))
  -> (S.EmbeddedValue B.Covered S.WithLocation -> Maybe (ev B.Covered S.WithLocation))
  -> Parser et ev a
  -> String
  -> Text
  -> m a
parse' eps ft fv (ParsecT p) n s =
  case runReader (M.runParserT p n s) (Env n eps ft fv) of
    Right a -> pure a
    Left e  -> throw $ Exception $ M.errorBundlePretty e

parse'' :: (MonadFail m, MonadReader (Env et ev) m) => Parser et ev a -> String -> Text -> m a
parse'' (ParsecT p) n s = do
  env <- ask
  case runReader (M.runParserT p n s) env of
    Right a -> pure a
    Left e  -> fail $ M.errorBundlePretty e

module' :: Parser et ev (S.WithLocation (Module et ev))
module' =
  M.label "module" $
    withPosition $
      P.parens $ do
        void $ P.textSymbol "module"
        S.Module <$> moduleName <*> list moduleName <*> list definition

list :: Parser et ev a -> Parser et ev (S.WithLocation [a])
list p =
  M.label "list" $
    withPosition $
      P.parens $ do
        void $ P.textSymbol "list"
        P.many p

list1 :: Parser et ev a -> Parser et ev (S.WithLocation (NonEmpty a))
list1 p =
  M.label "list1" $
    withPosition $
      P.parens $ do
        void $ P.textSymbol "list"
        fromMaybe X.unreachable . N.nonEmpty <$> P.some p

definition :: Parser et ev (S.WithLocation (Definition et ev))
definition =
  M.label "definition" $
    withPosition $
      P.parens $
        P.choice
          [ dataDefinition
          , foreignValueBind
          , valueBind
          , foreignTypeBind
          ]

dataDefinition :: Parser et ev (Definition et ev)
dataDefinition =
  M.label "dataDefinition" $ do
    void $ P.textSymbol "define"
    S.DataDefinition <$> identifier <*> list valueConstructor

valueConstructor :: Parser et ev (S.WithLocation (S.WithLocation S.Identifier, S.WithLocation [S.WithLocation (S.WithLocation S.Identifier, S.WithLocation Type)]))
valueConstructor =
  M.label "valueConstructor" $
    withPosition $
      P.choice
        [ (,) <$> identifier <*> withPosition (pure [])
        , P.parens $ (,) <$> identifier <*> list field
        ]

field :: Parser et ev (S.WithLocation (S.WithLocation S.Identifier, S.WithLocation Type))
field = M.label "field" $ withPosition $ P.parens $ (,) <$> identifier <*> typ

valueBind :: Parser et ev (Definition et ev)
valueBind =
  M.label "valueBind" $ do
    void $ P.textSymbol "bind-value"
    S.ValueBind <$> (S.ValueBindU <$> identifier <*> value)

foreignValueBind :: Parser et ev (Definition et ev)
foreignValueBind =
  M.label "foreignValueBind" $ do
    void $ P.textSymbol "bind-value-foreign"
    Env { embeddedParsers, isEmbeddedValue } <- ask
    let evps = (\EmbeddedParser { embeddedValueParser } -> embeddedValueParser) <$> embeddedParsers
    i <- identifier
    evs <- list $ P.parens $ P.choice evps
    t <- typ
    case mapMaybe (traverse isEmbeddedValue) $ copoint evs of
      [ev] -> pure $ S.ForeignValueBind i ev t
      _    -> fail "no or more than one embedded value parsers"

foreignTypeBind :: Parser et ev (Definition et ev)
foreignTypeBind =
  M.label "foreignTypeBind" $ do
    void $ P.textSymbol "bind-type-foreign"
    Env { embeddedParsers, isEmbeddedType } <- ask
    let etps = (\EmbeddedParser { embeddedTypeParser } -> embeddedTypeParser) <$> embeddedParsers
    i <- identifier
    ets <- list $ P.parens $ P.choice etps
    case mapMaybe (traverse isEmbeddedType) $ copoint ets of
      [et] -> pure $ S.ForeignTypeBind i et
      _    -> fail "no or more than one embedded type parsers"

identifier :: Parser et ev (S.WithLocation S.Identifier)
identifier =
  M.label "identifier" $
    P.token $
      withPosition $ do
        a <- asciiAlphabet
        b <- many $ P.choice [asciiAlphabet, P.digit]
        pure $ S.UserIdentifier $ T.pack $ a : b

eitherIdentifier :: Parser et ev (S.WithLocation S.EitherIdentifier)
eitherIdentifier =
  M.label "eitherIdentifier" $
    P.token $
      withPosition $ do
        is <- dotSeparatedIdentifier
        let n = S.UserIdentifier $ N.last is
        case N.nonEmpty (N.init is) of
          Nothing -> pure $ S.UnqualifiedIdentifier n
          Just m  -> pure $ S.QualifiedIdentifier $ S.GlobalIdentifier (S.ModuleName m) n

moduleName :: Parser et ev (S.WithLocation S.ModuleName)
moduleName = M.label "moduleName" $ fmap S.ModuleName <$> P.token (withPosition dotSeparatedIdentifier)

dotSeparatedIdentifier :: Parser et ev (N.NonEmpty Text)
dotSeparatedIdentifier =
  M.label "dotSeparatedIdentifier" $ P.sepByNonEmpty identifierSegment (P.char '.')

identifierSegment :: Parser et ev Text
identifierSegment = do
  a <- asciiAlphabet
  b <- many $ P.choice [asciiAlphabet, P.digit]
  pure $ T.pack $ a : b

value :: Parser et ev (S.WithLocation (Value et ev))
value =
  M.label "value'" $
    withPosition $
      fmap S.UntypedValue $
        withPosition $
          P.choice
            [ S.Variable <$> eitherIdentifier
            , S.Literal <$> literal
            , P.parens $
                P.choice
                  [ S.Function <$> function
                  , S.Application <$> application
                  , S.Procedure <$> procedure
                  , S.TypeAnnotation <$> typeAnnotation
                  , S.Let <$> (P.textSymbol "let" *> list definition) <*> value
                  , uncurry S.ForAll <$> forAll
                  ]
            ]

literal :: Parser et ev S.Literal
literal =
  M.label "literal" $
    P.choice
      [ P.try fraction
      , integer
      , S.String <$> string
      ]

application :: Parser et ev (Application et ev)
application =
  M.label "application" $ do
    void $ P.textSymbol "apply"
    S.ApplicationC <$> value <*> value

procedure :: Parser et ev (S.WithLocation (NonEmpty (S.WithLocation (ProcedureStep et ev))))
procedure =
  M.label "procedure" $ do
    void $ P.textSymbol "procedure"
    list1 step
  where
    step :: Parser et ev (S.WithLocation (ProcedureStep et ev))
    step =
      M.label "procedure.p" $
        withPosition $
          P.parens $
            P.choice
              [ do
                  void $ P.textSymbol "bind"
                  S.BindProcedureStep <$> identifier <*> value
              , do
                  void $ P.textSymbol "call"
                  S.CallProcedureStep <$> value
              ]

typeAnnotation :: Parser et ev (TypeAnnotation et ev)
typeAnnotation =
  M.label "typeAnnotation" $ do
    void $ P.textSymbol "type"
    S.TypeAnnotation' <$> value <*> typ

integer :: Parser et ev S.Literal
integer =
  M.label "integer" $ do
    P.token $
      P.choice
        [ P.text "0b" >> flip S.Integer 2 <$> M.binary
        , P.text "0o" >> flip S.Integer 8 <$> M.octal
        , P.text "0x" >> flip S.Integer 16 <$> M.hexadecimal
        , flip S.Integer 10 <$> M.decimal
        ]

fraction :: Parser et ev S.Literal
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

digits :: Word -> Parser et ev Text
digits b = do
  let bs = L.genericTake b ['0' .. '9'] ++ ((++) <$> id <*> (C.toUpper <$>) $ L.genericTake (b - 10) ['a' ..])
  T.pack <$> many (P.choice $ P.char <$> bs)

sign :: Parser et ev Bool
sign =  P.option True $ P.text "-" $> False <|> P.text "+" $> True

string :: Parser et ev Text
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

function :: Parser et ev (Function et ev)
function =
  M.label "function" $ do
    void $ P.textSymbol "function"
    S.FunctionC <$> identifier <*> typ <*> value

typ :: Parser et ev (S.WithLocation Type)
typ =
  M.label "type" $
    withPosition $
      P.choice
        [ S.TypeVariable <$> eitherIdentifier
        , P.parens $
            P.choice
              [ S.FunctionType <$> functionType
              , uncurry S.TypeApplication <$> typeApplication
              , S.ProcedureType <$> procedureType
              ]
        ]

functionType :: Parser et ev FunctionType
functionType =
  M.label "functionType" $ do
    void $ P.textSymbol "function"
    S.FunctionTypeC <$> typ <*> typ

typeApplication :: Parser et ev (S.WithLocation Type, S.WithLocation Type)
typeApplication =
  M.label "typeApplication" $ do
    void $ P.textSymbol "apply"
    (,) <$> typ <*> typ

procedureType :: Parser et ev (S.WithLocation Type)
procedureType =
  M.label "procedureType" $ do
    void $ P.textSymbol "procedure"
    typ

forAll :: Parser et ev (S.WithLocation S.Identifier, S.WithLocation (Value et ev))
forAll =
  M.label "forAll" $ do
    void $ P.textSymbol "for-all"
    (,) <$> identifier <*> value

doubleQuote :: Parser et ev a -> Parser et ev a
doubleQuote = M.label "doubleQuote" . (`P.surroundedBy` P.text "\"")

asciiAlphabet :: Parser et ev Char
asciiAlphabet = P.choice [asciiUpper, asciiLower]

asciiUpper :: Parser et ev Char
asciiUpper = P.choice $ P.char <$> ['A' .. 'Z']

asciiLower :: Parser et ev Char
asciiLower = P.choice $ P.char <$> ['a' .. 'z']

withPosition :: Parser et ev a -> Parser et ev (S.WithLocation a)
withPosition p = do
  Env { filePath } <- ask
  M.SourcePos _ beginLine beginColumn <- M.getSourcePos
  a <- p
  M.SourcePos _ endLine endColumn <- M.getSourcePos
  pure $
    S.WithLocation
      ( S.Location
          filePath
          (S.Position (fromIntegral $ M.unPos beginLine) (fromIntegral $ M.unPos beginColumn))
          (S.Position (fromIntegral $ M.unPos endLine) (fromIntegral $ M.unPos endColumn))
      )
      a

data EmbeddedParser et ev =
  EmbeddedParser
    { embeddedValueParser :: Parser et ev (S.WithLocation EmbeddedValue)
    , embeddedTypeParser  :: Parser et ev (S.WithLocation EmbeddedType)
    }
  deriving Generic

newtype Exception
  = Exception String
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e
