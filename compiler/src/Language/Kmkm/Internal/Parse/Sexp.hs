{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Kmkm.Internal.Parse.Sexp
  ( parse
  , parseText
  , parseSexp
  , parseSexp'
  , module'
  , definition
  , dataDefinition
  , valueBind
  , foreignTypeBind
  , foreignValueBind
  , integer
  , fraction
  , string
  , identifier
  , symbol
  , list
  , stringSingleDoubleQuotationText
  , stringTripleDoubleQuotationText
  , SexpParser
  , EmbeddedParser (..)
  , Env (..)
  , Exception (..)
  , MonadAlternativeError
  ) where

import qualified Language.Kmkm.Internal.Exception                                                 as X
import           Language.Kmkm.Internal.Parse.Location                                            (withLocation)
import           Language.Kmkm.Internal.Syntax.Core.Common                                        (EmbeddedType,
                                                                                                   EmbeddedValue)
import qualified Language.Kmkm.Internal.Syntax.Core.Common                                        as S
import qualified Language.Kmkm.Internal.Syntax.Core.NameUnresolved.Untyped.Curried.LambdaUnlifted as S
import qualified Language.Kmkm.Internal.Syntax.Sexp                                               as SS

import           Control.Applicative        (Alternative (many, some), liftA2, (<|>))
import           Control.Exception.Safe     (MonadCatch, MonadThrow, throw)
import qualified Control.Exception.Safe     as E
import           Control.Monad              (void)
import           Control.Monad.Except       (Except, ExceptT, MonadError (throwError), liftEither, runExcept)
import           Control.Monad.Reader       (MonadReader (ask), ReaderT (runReaderT))
import           Data.Bool                  (bool)
import qualified Data.Char                  as C
import           Data.Copointed             (Copointed (copoint))
import           Data.Foldable              (for_)
import           Data.Functor               (($>))
import           Data.Functor.F             (F (F))
import           Data.Functor.Identity      (Identity)
import           Data.Functor.With          (MayHave, With)
import qualified Data.Functor.With          as W
import qualified Data.Kind                  as K
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as N
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Traversable           (for)
import qualified Data.Typeable              as Y
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           GHC.Stack                  (HasCallStack)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as M
import           Text.Megaparsec.Parsers    (ParsecT (unParsecT))
import qualified Text.Parser.Char           as P
import qualified Text.Parser.Combinators    as P
import qualified Text.Parser.Token          as P

type SexpParser f m a = F f (SS.Sexp f) -> m a

type Env :: ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Env et ev f =
  Env
    { filePath        :: FilePath
    , embeddedParsers :: [EmbeddedParser f]
    , isEmbeddedType  :: F f (EmbeddedType f) -> Maybe (F f (et f))
    , isEmbeddedValue :: F f (EmbeddedValue f) -> Maybe (F f (ev f))
    }

parse
  :: MonadCatch m
  => [EmbeddedParser (With S.Location)] -- ^ Embedded parser.
  -> (F (With S.Location) (EmbeddedType (With S.Location)) -> Maybe (F (With S.Location) (et (With S.Location)))) -- ^ Embedded type filter.
  -> (F (With S.Location) (EmbeddedValue (With S.Location)) -> Maybe (F (With S.Location) (ev (With S.Location)))) -- ^ Embedded value filter.
  -> FilePath
  -> Text -- ^ Input.
  -> m (F (With S.Location) (S.Module et ev (With S.Location)))
parse embeddedParsers isEmbeddedType isEmbeddedValue filePath input = do
  s <- parseText filePath input
  parseSexp embeddedParsers isEmbeddedType isEmbeddedValue filePath s

parseText :: MonadThrow m => FilePath -> Text -> m (F (With S.Location) (SS.Sexp (With S.Location)))
parseText filePath input =
  case M.runParser (unParsecT $ sexp filePath <* P.eof) filePath input of
    Right s -> return s
    Left e  -> throw [TextException $ M.errorBundlePretty e]

parseInParsec :: MonadFail m => FilePath -> ParsecT Void Text Identity a -> Text -> m a
parseInParsec filePath parser input =
  case M.runParser (unParsecT $ parser <* P.eof) filePath input of
    Right s -> return s
    Left e  -> fail $ M.errorBundlePretty e

parseSexp
  :: MonadCatch m
  => [EmbeddedParser (With S.Location)] -- ^ Embedded parser.
  -> (F (With S.Location) (EmbeddedType (With S.Location)) -> Maybe (F (With S.Location) (et (With S.Location)))) -- ^ Embedded type filter.
  -> (F (With S.Location) (EmbeddedValue (With S.Location)) -> Maybe (F (With S.Location) (ev (With S.Location)))) -- ^ Embedded value filter.
  -> FilePath
  -> F (With S.Location) (SS.Sexp (With S.Location))
  -> m (F (With S.Location) (S.Module et ev (With S.Location)))
parseSexp embeddedParsers isEmbeddedType isEmbeddedValue filePath s =
  case runExcept $ flip runReaderT Env { filePath, embeddedParsers, isEmbeddedType, isEmbeddedValue } $ module' s of
    Right a -> pure a
    Left e  -> throw e

parseSexp'
  :: MonadCatch m
  => (F f (SS.Sexp f) -> ReaderT (Env et ev f) (Except [Exception]) a)
  -> [EmbeddedParser f]
  -> (F f (EmbeddedType f) -> Maybe (F f (et f)))
  -> (F f (EmbeddedValue f) -> Maybe (F f (ev f)))
  -> FilePath
  -> F f (SS.Sexp f)
  -> m a
parseSexp' p embeddedParsers isEmbeddedType isEmbeddedValue filePath s =
  case runExcept $ flip runReaderT Env { filePath, embeddedParsers, isEmbeddedType, isEmbeddedValue } $ p s of
    Right a -> pure a
    Left e  -> throw e

sexp :: FilePath -> ParsecT Void Text Identity (F (With S.Location) (SS.Sexp (With S.Location)))
sexp filePath =
  fmap F $
    P.token $
      withLocation filePath $
        P.choice
          [ SS.List <$> P.parens (many $ sexp filePath)
          , SS.Atom <$> atom
          ]
  where
    atom :: ParsecT Void Text Identity Text
    atom =
      P.choice
        [ ("\"\"\"" <>) . (<> "\"\"\"") <$> stringTripleDoubleQuotationText
        , ("\"" <>) . (<> "\"") <$> stringSingleDoubleQuotationText
        , T.pack <$> some (P.satisfy $ not . (C.isSpace <||> (== '(') <||> (== ')')))
        ]
    (<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    (<||>) = liftA2 (||)

module' :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Module et ev f))
module' s =
  for s $ \case
    SS.List [sm, sn, sis, sds] -> do
      symbol "module" "module'" sm
      S.Module <$> moduleName sn <*> list moduleName sis <*> list definition sds
    s' -> throwError [SexpException ("a list with 4 elements expected, but got " ++ abstractMessage s') "module'" $ W.mayGet s]

moduleName :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f S.ModuleName)
moduleName s = fmap S.ModuleName <$> atom "moduleName" dotSeparatedIdentifier s

definition :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Definition et ev f))
definition s =
  P.choice
    [ dataDefinition s
    , foreignValueBind s
    , valueBind s
    , foreignTypeBind s
    ]

foreignValueBind :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Definition et ev f))
foreignValueBind s =
  for s $ \case
    SS.List [sb, si, sevs, st] -> do
      symbol "bind-value-foreign" "foreignValueBind" sb
      i <- identifier si
      Env { embeddedParsers, isEmbeddedValue } <- ask
      let evps = (\EmbeddedParser { embeddedValueParser } -> embeddedValueParser) <$> embeddedParsers
      evs <- list (runEmbeddedParsers evps) sevs
      t <- typ st
      case mapMaybe isEmbeddedValue $ copoint evs of -- TODO 選択は別ステップにする
        [ev] -> pure $ S.ForeignValueBind i ev t
        _    -> throwError [SexpException "no or more than one embedded value parsers" "foreignValueBind" $ W.mayGet s]
    s' -> throwError [SexpException ("a list with 4 elements expected, but got " ++ abstractMessage s') "foreignValueBind" $ W.mayGet s]

foreignTypeBind :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Definition et ev f))
foreignTypeBind s =
  for s $ \case
    SS.List [sb, si, sets] -> do
      symbol "bind-type-foreign" "foreignTypeBind" sb
      i <- identifier si
      Env { embeddedParsers, isEmbeddedType } <- ask
      let etps = (\EmbeddedParser { embeddedTypeParser } -> embeddedTypeParser) <$> embeddedParsers
      ets <- list (runEmbeddedParsers etps) sets
      case mapMaybe isEmbeddedType $ copoint ets of
        [et] -> pure $ S.ForeignTypeBind i et
        _    -> throwError [SexpException "no or more than one embedded type parsers" "foreignTypeBind" $ W.mayGet s]
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "foreignTypeBind" $ W.mayGet s]

runEmbeddedParsers :: (MonadAlternativeError [Exception] m, MayHave S.Location f) => [F f a -> Either [Exception] c] -> F f a -> m c
runEmbeddedParsers [] s   = throwError [SexpException "no embedded-parser" "runEmbeddedParsers" $ W.mayGet s]
runEmbeddedParsers evps s = P.choice $ liftEither . ($ s) <$> evps

dataDefinition :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Definition et ev f))
dataDefinition s =
  for s $ \case
    SS.List [sd, si, r] -> do
      symbol "define" "dataDefinition" sd
      S.DataDefinition <$> identifier si <*> dataRepresentation r
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "dataDefinition" $ W.mayGet s]

dataRepresentation :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.DataRepresentation et ev f))
dataRepresentation s =
  for s $ \s' ->
    P.choice
      [ case s' of
          SS.List [sk, si, sr] -> do
            symbol "for-all" "dataRepresentation" sk
            S.ForAllData <$> identifier si <*> dataRepresentation sr
          _ -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "dataRepresentation" $ W.mayGet s]
      , S.ValueConstructorsData <$> list valueConstructor s
      ]

valueConstructor :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.ValueConstructor et ev f))
valueConstructor s =
  for s $ \case
    SS.Atom _         -> S.ValueConstructor <$> identifier s <*> pure ([] <$ s)
    SS.List [si, sfs] -> S.ValueConstructor <$> identifier si <*> list field sfs
    s'                -> throwError [SexpException ("an atom or a list with 2 elements expected, but got " ++ abstractMessage s') "valueConstructor" $ W.mayGet s]

field :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Field et ev f))
field s =
  for s $ \case
    SS.List [si, st] -> S.Field <$> identifier si <*> typ st
    s'               -> throwError [SexpException ("a list with 2 elements expected, but got " ++ abstractMessage s') "field" $ W.mayGet s]

typ :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Type f))
typ s =
  for s $ \case
    SS.Atom _ -> S.TypeVariable <$> eitherIdentifier s
    l@(SS.List _) ->
      P.choice
        [ case l of
            SS.List [si, st1, st2] -> do
              symbol "function" "functionType" si
              S.FunctionType <$> typ st1 <*> typ st2
            l' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage l') "functionType" $ W.mayGet s]
        , case l of
            SS.List [sk, st1, st2] -> do
              symbol "apply" "typeApplication" sk
              S.TypeApplication <$> typ st1 <*> typ st2
            l' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage l') "typeApplication" $ W.mayGet s]
        , S.ProcedureType <$> procedureType s
        ]

procedureType :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Type f))
procedureType s =
  case copoint s of
    SS.List [sk, st] -> do
      symbol "procedure" "procedureType" sk
      typ st
    s' -> throwError [SexpException ("a list with 2 elements expected, but got " ++ abstractMessage s') "procedureType" $ W.mayGet s]

valueBind :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Definition et ev f))
valueBind s =
  for s $ \case
    SS.List [s1, s2, s3] ->
      case copoint s1 of
        SS.Atom "bind-value" -> S.ValueBind <$> identifier s2 <*> value s3
        SS.Atom n -> throwError [SexpException ("\"bind-value\" expected, but got \"" <> T.unpack n <> "\"") "valueBind" $ W.mayGet s1]
        s1' -> throwError [SexpException ("an atom of \"bind-value\" expected, but got " ++ abstractMessage s1') "valueBind" $ W.mayGet s1]
    s' -> throwError [SexpException ("a list with 3 elements expected, but got '" ++ show (abstractMessage s') ++ "'") "valueBind" $ W.mayGet s]

literal :: (MonadAlternativeError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f S.Literal)
literal s =
  P.choice
    [ fraction s
    , integer s
    , fmap S.String <$> string s
    ]

integer :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f S.Literal)
integer =
  atom "integer" $
    P.choice
      [ P.text "0b" >> flip S.Integer 2 <$> M.binary
      , P.text "0o" >> flip S.Integer 8 <$> M.octal
      , P.text "0x" >> flip S.Integer 16 <$> M.hexadecimal
      , flip S.Integer 10 <$> M.decimal
      ]

fraction :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f S.Literal)
fraction =
  atom "fraction" $ do hexadecimal <|> decimal
  where
    hexadecimal = do
      void $ P.text "0x"
      istr <- digits 16
      fstr <- P.option "" $ P.text "." >> digits 16
      let
        fractionDigits :: Word
        fractionDigits = fromIntegral $ T.length fstr
      significand <- parseInParsec "fraction.hexadecimal.significand" M.hexadecimal $ istr <> fstr
      void $ P.text "p"
      epos <- sign
      estr <- digits 16
      exponent <- if T.null estr then pure 0 else parseInParsec "fraction.hexadecimal.exponent" M.decimal estr
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
      significand <- parseInParsec "fraction.decimal.significand" M.decimal $ istr <> fstr
      exponent <- if T.null estr then pure 0 else parseInParsec "fraction.decimal.exponent" M.decimal estr
      pure $ S.Fraction significand fractionDigits (sign' epos exponent) 10
    sign' :: Num n => Bool -> n -> n
    sign' = bool negate id

    digits :: Word -> ParsecT Void Text Identity Text
    digits b = do
      let bs = L.genericTake b ['0' .. '9'] ++ ((++) <$> id <*> (C.toUpper <$>) $ L.genericTake (b - 10) ['a' ..])
      T.pack <$> many (P.choice $ P.char <$> bs)

    sign :: ParsecT Void Text Identity Bool
    sign = P.option True $ P.text "-" $> False <|> P.text "+" $> True

string :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f Text)
string = atom "string" $ stringTripleDoubleQuotationText <|> stringSingleDoubleQuotationText

stringSingleDoubleQuotationText :: ParsecT Void Text Identity Text
stringSingleDoubleQuotationText = do
  M.label "stringSingleDoubleQuotationText" $ do
    P.token $
        doubleQuote $
          fmap mconcat $
            many $
              P.choice
                [ P.text "\\\"" $> "\""
                , T.singleton <$> P.notChar '"'
                ]

stringTripleDoubleQuotationText :: ParsecT Void Text Identity Text
stringTripleDoubleQuotationText = do
  M.label "stringTripleDoubleQuotationText" $ do
    P.token $
        do
          void $ P.text "\"\"\""
          let
            -- | rightest match of “"""”
            go
              :: Int -- ^ the length of sequence of “"”
              -> ParsecT Void Text Identity Text
            go n = do
              c <- P.anyChar
              case c of
                '"' ->
                  if n == 2
                    then do
                      P.choice
                        [ do
                            P.notFollowedBy $ P.char '"'
                            pure T.empty
                        , (T.singleton c <>) <$> go n
                        ]
                    else go (n + 1)
                _ ->
                  if n == 0
                    then (T.singleton c <>) <$> go n
                    else ((T.replicate n "\"" <> T.singleton c) <>) <$> go 0
          go 0

doubleQuote :: ParsecT Void Text Identity a -> ParsecT Void Text Identity a
doubleQuote = M.label "doubleQuote" . (`P.surroundedBy` P.text "\"")

asciiAlphabet :: ParsecT Void Text Identity Char
asciiAlphabet = P.choice [asciiUpper, asciiLower]

asciiUpper :: ParsecT Void Text Identity Char
asciiUpper = P.choice $ P.char <$> ['A' .. 'Z']

asciiLower :: ParsecT Void Text Identity Char
asciiLower = P.choice $ P.char <$> ['a' .. 'z']

eitherIdentifier :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f S.EitherIdentifier)
eitherIdentifier =
  atom "eitherIdentifier" $ do
    is <- dotSeparatedIdentifier
    let n = S.UserIdentifier $ N.last is
    case N.nonEmpty (N.init is) of
      Nothing -> pure $ S.UnqualifiedIdentifier n
      Just m  -> pure $ S.QualifiedIdentifier $ S.GlobalIdentifier (S.ModuleName m) n

dotSeparatedIdentifier :: ParsecT Void Text Identity (N.NonEmpty Text)
dotSeparatedIdentifier =
  M.label "dotSeparatedIdentifier" $ P.sepByNonEmpty identifierSegment (P.char '.')

identifierSegment :: ParsecT Void Text Identity Text
identifierSegment = do
  a <- asciiAlphabet
  b <- many $ P.choice [asciiAlphabet, P.digit]
  pure $ T.pack $ a : b

identifier :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f S.Identifier)
identifier =
  atom "identifier" $ do
    a <- asciiAlphabet
    b <- many $ P.choice [asciiAlphabet, P.digit]
    pure $ S.UserIdentifier $ T.pack $ a : b

value :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f, HasCallStack) => SexpParser f m (F f (S.Value et ev f))
value s =
  P.choice
    [ (<$ s) . S.Variable <$> eitherIdentifier s
    , (<$ s) . S.Literal <$> literal s
    , function s
    , application s
    , (<$ s) . S.Procedure <$> procedure s
    , typeAnnotation s
    , fmap (uncurry S.Let) <$> let' s
    , fmap (uncurry S.ForAllValue) <$> forAll s
    , fmap (uncurry S.Instantiation) <$> instantiation s
    ]

procedure :: (MonadReader (Env et ev f) m, MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f, HasCallStack) => SexpParser f m (F f (NonEmpty (F f (S.ProcedureStep et ev f))))
procedure s =
  case copoint s of
    SS.List [sk, sss] -> do
      symbol "procedure" "procedure" sk
      list1 step sss
    s' -> throwError [SexpException ("a list with 2 elements expected, but got " ++ abstractMessage s') "procedure" $ W.mayGet s]
  where
    step :: (MonadReader (Env et ev f) m, MonadAlternativeError [Exception] m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.ProcedureStep et ev f))
    step s =
      for s $ \case
        SS.List [sk, si, sv] -> do
          symbol "bind" "procedure.step" sk
          S.BindProcedureStep <$> identifier si <*> value sv
        SS.List [sk, sv] -> do
          symbol "call" "procedure.step" sk
          S.CallProcedureStep <$> value sv
        s' -> throwError [SexpException ("a list with 2 or 3 elements expected, but got " ++ abstractMessage s') "procedure.step" $ W.mayGet s]

let' :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (F f [F f (S.Definition et ev f)], F f (S.Value et ev f)))
let' s =
  for s $ \case
    SS.List [sk, sds, sv] -> do
      symbol "let" "let'" sk
      (,) <$> list definition sds <*> value sv
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "let'" $ W.mayGet s]

forAll :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (F f S.Identifier, F f (S.Value et ev f)))
forAll s =
  for s $ \case
    SS.List [sk, si, sv] -> do
      symbol "for-all" "forAll" sk
      (,) <$> identifier si <*> value sv
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "forAll" $ W.mayGet s]

typeAnnotation :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Value et ev f))
typeAnnotation s =
  for s $ \case
    SS.List [sk, sv, st] -> do
      symbol "type" "typeAnnotation" sk
      S.TypeAnnotation <$> value sv <*> typ st
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "typeAnnotation" $ W.mayGet s]

application :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Value et ev f))
application s =
  for s $ \case
    SS.List [sk, sv1, sv2] -> do
      symbol "apply" "application" sk
      S.Application <$> value sv1 <*> value sv2
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "application" $ W.mayGet s]

function :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (S.Value et ev f))
function s =
  for s $ \case
    SS.List [sk, si, st, sv] -> do
      symbol "function" "function" sk
      S.Function <$> identifier si <*> typ st <*> value sv
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "function" $ W.mayGet s]

instantiation :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, MayHave S.Location f) => SexpParser f m (F f (F f (S.Value et ev f), F f (S.Type f)))
instantiation s =
  for s $ \case
    SS.List [sk, sv, st] -> do
      symbol "instantiate" "instantiation" sk
      (,) <$> value sv <*> typ st
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "instantiation" $ W.mayGet s]

list :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => SexpParser f m (F f a) -> SexpParser f m (F f [F f a])
list p s =
  for s $ \case
    SS.List (l : ss) -> do
      symbol "list" "list" l
      traverse p ss
    s' -> throwError [SexpException ("a list with some elements and the first element \"list\" expected, but got " ++ abstractMessage s') "list" $ W.mayGet s]

list1 :: (MonadError [Exception] m, Traversable f, MayHave S.Location f, HasCallStack) => SexpParser f m (F f a) -> SexpParser f m (F f (NonEmpty (F f a)))
list1 p s =
  for s $ \case
    SS.List (l : ss) -> do
      symbol "list" "list1" l
      fromMaybe (X.unreachable "empty") . N.nonEmpty <$> traverse p ss
    s' -> throwError [SexpException ("a list with some elements and the first element \"list\" expected, but got " ++ abstractMessage s') "list1" $ W.mayGet s]

atom :: (MonadError [Exception] m, Traversable f, MayHave S.Location f) => String -> ParsecT Void Text Identity a -> SexpParser f m (F f a)
atom label parser s =
  for s $ \case
    SS.Atom t ->
      case M.runParser (unParsecT $ parser <* P.eof) label t of
        Right i -> pure i
        Left e  -> throwError [SexpException (M.errorBundlePretty e) label $ W.mayGet s]
    s' -> throwError [SexpException ("an atom expected, but got " ++ abstractMessage s') label $ W.mayGet s]

symbol :: (MonadError [Exception] m, Foldable f, MayHave S.Location f) => Text -> String -> SexpParser f m ()
symbol sym label s =
  for_ s $ \case
    SS.Atom sym'
      | sym == sym' -> pure ()
      | otherwise -> throwError [SexpException ("\"" ++ T.unpack sym ++ "\" expected, but got \"" ++ T.unpack sym' ++ "\"") label $ W.mayGet s]
    s' -> throwError [SexpException ("an atom expected, but got " ++ abstractMessage s') label $ W.mayGet s]

abstractMessage :: SS.Sexp f -> String
abstractMessage (SS.Atom m)  = T.unpack $ "an atom of \"" <> m <> "\""
abstractMessage (SS.List ss) = "a list with " ++ show (length ss) ++ " element(s)"

data EmbeddedParser f =
  EmbeddedParser
    { embeddedValueParser :: F f (SS.Sexp f) -> Either [Exception] (F f (EmbeddedValue f))
    , embeddedTypeParser  :: F f (SS.Sexp f) -> Either [Exception] (F f (EmbeddedType f))
    }
  deriving Generic

data Exception
  = TextException String
  | SexpException String String (Maybe S.Location)
  deriving (Show, Eq, Generic)

instance E.Exception [Exception] where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e

-- | Laws are
--
-- @
--   throwError e \<|\> m ≡ m
--   m \<|\> throwError e ≡ m
-- @
class (Alternative m, MonadError e m) => MonadAlternativeError e m

instance (Monad m, Monoid e) => MonadAlternativeError e (ExceptT e m)

instance MonadAlternativeError e m => MonadAlternativeError e (ReaderT r m)
