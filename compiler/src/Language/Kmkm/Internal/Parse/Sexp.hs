{-# LANGUAGE DataKinds                #-}
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

import           Barbies.Bare                          (Covered)
import qualified Barbies.Bare                          as B
import           Control.Applicative                   (Alternative (many, some), liftA2, (<|>))
import           Control.Exception.Safe                (MonadCatch, MonadThrow, throw)
import qualified Control.Exception.Safe                as E
import           Control.Monad                         (void)
import           Control.Monad.Except                  (Except, ExceptT, MonadError (throwError), liftEither, runExcept)
import           Control.Monad.Reader                  (MonadReader (ask), ReaderT (runReaderT))
import           Data.Bool                             (bool)
import qualified Data.Char                             as C
import           Data.Copointed                        (Copointed (copoint))
import           Data.Foldable                         (for_)
import           Data.Functor                          (($>))
import           Data.Functor.Identity                 (Identity)
import qualified Data.Kind                             as K
import qualified Data.List                             as L
import           Data.List.NonEmpty                    (NonEmpty)
import qualified Data.List.NonEmpty                    as N
import           Data.Maybe                            (fromMaybe, mapMaybe)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Traversable                      (for)
import qualified Data.Typeable                         as Y
import           Data.Void                             (Void)
import           GHC.Generics                          (Generic)
import qualified Language.Kmkm.Internal.Exception      as X
import           Language.Kmkm.Internal.Parse.Location (withLocation)
import qualified Language.Kmkm.Internal.Syntax         as S
import qualified Language.Kmkm.Internal.Syntax.Sexp    as SS
import qualified Text.Megaparsec                       as M
import qualified Text.Megaparsec.Char.Lexer            as M
import           Text.Megaparsec.Parsers               (ParsecT (unParsecT))
import qualified Text.Parser.Char                      as P
import qualified Text.Parser.Combinators               as P
import qualified Text.Parser.Token                     as P

type Module et ev = S.Module 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type Definition et ev = S.Definition 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type ValueConstructor et ev = S.ValueConstructor 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted et ev B.Covered

type Field et ev = S.Field 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted et ev B.Covered

type ValueBind et ev = S.ValueBind 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type Type = S.Type 'S.NameUnresolved 'S.Curried B.Covered

type FunctionType = S.FunctionType 'S.NameUnresolved 'S.Curried B.Covered

type Value et ev = S.Value 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type Function et ev = S.Function 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type Application et ev = S.Application 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type TypeAnnotation et ev = S.TypeAnnotation 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type ProcedureStep et ev  = S.ProcedureStep 'S.NameUnresolved 'S.Curried 'S.LambdaUnlifted 'S.Untyped et ev B.Covered

type EmbeddedValue = S.EmbeddedValue B.Covered

type EmbeddedType = S.EmbeddedType B.Covered

type Sexp = SS.Sexp Covered

type SexpParser f m a = f (Sexp f) -> m a

type Env :: (K.Type -> (K.Type -> K.Type) -> K.Type) -> (K.Type -> (K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Type
data Env et ev f =
  Env
    { filePath        :: FilePath
    , embeddedParsers :: [EmbeddedParser f]
    , isEmbeddedType  :: f (EmbeddedType f) -> Maybe (f (et B.Covered f))
    , isEmbeddedValue :: f (EmbeddedValue f) -> Maybe (f (ev B.Covered f))
    }

parse
  :: MonadCatch m
  => [EmbeddedParser S.WithLocation] -- ^ Embedded parser.
  -> (S.WithLocation (EmbeddedType S.WithLocation) -> Maybe (S.WithLocation (et B.Covered S.WithLocation))) -- ^ Embedded type filter.
  -> (S.WithLocation (EmbeddedValue S.WithLocation) -> Maybe (S.WithLocation (ev B.Covered S.WithLocation))) -- ^ Embedded value filter.
  -> FilePath
  -> Text -- ^ Input.
  -> m (S.WithLocation (Module et ev S.WithLocation))
parse embeddedParsers isEmbeddedType isEmbeddedValue filePath input = do
  s <- parseText filePath input
  parseSexp embeddedParsers isEmbeddedType isEmbeddedValue filePath s

parseText :: MonadThrow m => FilePath -> Text -> m (S.WithLocation (Sexp S.WithLocation))
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
  => [EmbeddedParser S.WithLocation] -- ^ Embedded parser.
  -> (S.WithLocation (EmbeddedType S.WithLocation) -> Maybe (S.WithLocation (et B.Covered S.WithLocation))) -- ^ Embedded type filter.
  -> (S.WithLocation (EmbeddedValue S.WithLocation) -> Maybe (S.WithLocation (ev B.Covered S.WithLocation))) -- ^ Embedded value filter.
  -> FilePath
  -> S.WithLocation (Sexp S.WithLocation)
  -> m (S.WithLocation (Module et ev S.WithLocation))
parseSexp embeddedParsers isEmbeddedType isEmbeddedValue filePath s =
  case runExcept $ flip runReaderT Env { filePath, embeddedParsers, isEmbeddedType, isEmbeddedValue } $ module' s of
    Right a -> pure a
    Left e  -> throw e

parseSexp'
  :: MonadCatch m
  => (f (Sexp f) -> ReaderT (Env et ev f) (Except [Exception]) a)
  -> [EmbeddedParser f]
  -> (f (EmbeddedType f) -> Maybe (f (et B.Covered f)))
  -> (f (EmbeddedValue f) -> Maybe (f (ev B.Covered f)))
  -> FilePath
  -> f (Sexp f)
  -> m a
parseSexp' p embeddedParsers isEmbeddedType isEmbeddedValue filePath s =
  case runExcept $ flip runReaderT Env { filePath, embeddedParsers, isEmbeddedType, isEmbeddedValue } $ p s of
    Right a -> pure a
    Left e  -> throw e

sexp :: FilePath -> ParsecT Void Text Identity (S.WithLocation (Sexp S.WithLocation))
sexp filePath = do
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

module' :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Module et ev f))
module' s =
  for s $ \case
    SS.List [sm, sn, sis, sds] -> do
      symbol "module" "module'" sm
      S.Module <$> moduleName sn <*> list moduleName sis <*> list definition sds
    s' -> throwError [SexpException ("a list with 4 elements expected, but got " ++ abstractMessage s') "module'" $ S.location s]

moduleName :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f S.ModuleName)
moduleName s = fmap S.ModuleName <$> atom "moduleName" dotSeparatedIdentifier s

definition :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Definition et ev f))
definition s =
  P.choice
    [ dataDefinition s
    , foreignValueBind s
    , fmap S.ValueBind <$> valueBind s
    , foreignTypeBind s
    ]

foreignValueBind :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Definition et ev f))
foreignValueBind s =
  for s $ \case
    SS.List [sb, si, sevs, st] -> do
      symbol "bind-value-foreign" "foreignValueBind" sb
      i <- identifier si
      Env { embeddedParsers, isEmbeddedValue } <- ask
      let evps = (\EmbeddedParser { embeddedValueParser } -> embeddedValueParser) <$> embeddedParsers
      evs <- list (runEmbeddedParsers evps) sevs
      t <- typ st
      case mapMaybe isEmbeddedValue $ copoint evs of -- 選択は別ステップにする
        [ev] -> pure $ S.ForeignValueBind i ev t
        _ -> throwError [SexpException "no or more than one embedded value parsers" "foreignValueBind" $ S.location s]
    s' -> throwError [SexpException ("a list with 4 elements expected, but got " ++ abstractMessage s') "foreignValueBind" $ S.location s]

foreignTypeBind :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Definition et ev f))
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
        _    -> throwError [SexpException "no or more than one embedded type parsers" "foreignTypeBind" $ S.location s]
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "foreignTypeBind" $ S.location s]

runEmbeddedParsers :: (MonadAlternativeError [Exception] m, S.HasLocation f) => [f a -> Either [Exception] c] -> f a -> m c
runEmbeddedParsers [] s   = throwError [SexpException "no embedded-parser" "runEmbeddedParsers" $ S.location s]
runEmbeddedParsers evps s = P.choice $ liftEither . ($ s) <$> evps

dataDefinition :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Definition et ev f))
dataDefinition s =
  for s $ \case
    SS.List [sd, si, sc] -> do
      symbol "define" "dataDefinition" sd
      S.DataDefinition <$> identifier si <*> list valueConstructor sc
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "dataDefinition" $ S.location s]

valueConstructor :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (ValueConstructor et ev f))
valueConstructor s =
  for s $ \case
    SS.Atom _         -> S.ValueConstructor <$> identifier s <*> pure ([] <$ s)
    SS.List [si, sfs] -> S.ValueConstructor <$> identifier si <*> list field sfs
    s'                -> throwError [SexpException ("an atom or a list with 2 elements expected, but got " ++ abstractMessage s') "valueConstructor" $ S.location s]

field :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Field et ev f))
field s =
  for s $ \case
    SS.List [si, st] -> S.Field <$> identifier si <*> typ st
    s'               -> throwError [SexpException ("a list with 2 elements expected, but got " ++ abstractMessage s') "field" $ S.location s]

typ :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Type f))
typ s =
  for s $ \case
    SS.Atom _ -> S.TypeVariable <$> eitherIdentifier s
    l@(SS.List _) ->
      P.choice
        [ S.FunctionType <$> functionType s
        , case l of
            SS.List [sk, st1, st2] -> do
              symbol "apply" "typeApplication" sk
              S.TypeApplication <$> typ st1 <*> typ st2
            l' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage l') "typeApplication" $ S.location s]
        , S.ProcedureType <$> procedureType s
        ]

functionType :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (FunctionType f))
functionType s =
  for s $ \case
    SS.List [si, st1, st2] -> do
      symbol "function" "functionType" si
      S.FunctionTypeC <$> typ st1 <*> typ st2
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "functionType" $ S.location s]

procedureType :: (MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Type f))
procedureType s =
  case copoint s of
    SS.List [sk, st] -> do
      symbol "procedure" "procedureType" sk
      typ st
    s' -> throwError [SexpException ("a list with 2 elements expected, but got " ++ abstractMessage s') "procedureType" $ S.location s]

valueBind :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (ValueBind et ev f))
valueBind s =
  for s $ \case
    SS.List [s1, s2, s3] ->
      case copoint s1 of
        SS.Atom "bind-value" -> S.ValueBindU <$> identifier s2 <*> value s3
        SS.Atom n -> throwError [SexpException ("\"bind-value\" expected, but got \"" <> T.unpack n <> "\"") "valueBind" $ S.location s1]
        s1' -> throwError [SexpException ("an atom of \"bind-value\" expected, but got " ++ abstractMessage s1') "valueBind" $ S.location s1]
    s' -> throwError [SexpException ("a list with 3 elements expected, but got '" ++ show (abstractMessage s') ++ "'") "valueBind" $ S.location s]

literal :: (MonadAlternativeError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f S.Literal)
literal s =
  P.choice
    [ fraction s
    , integer s
    , fmap S.String <$> string s
    ]

integer :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f S.Literal)
integer =
  atom "integer" $
    P.choice
      [ P.text "0b" >> flip S.Integer 2 <$> M.binary
      , P.text "0o" >> flip S.Integer 8 <$> M.octal
      , P.text "0x" >> flip S.Integer 16 <$> M.hexadecimal
      , flip S.Integer 10 <$> M.decimal
      ]

fraction :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f S.Literal)
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

string :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f Text)
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

eitherIdentifier :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f S.EitherIdentifier)
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

identifier :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f S.Identifier)
identifier =
  atom "identifier" $ do
    a <- asciiAlphabet
    b <- many $ P.choice [asciiAlphabet, P.digit]
    pure $ S.UserIdentifier $ T.pack $ a : b

value :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Value et ev f))
value s =
  (<$ s) . S.UntypedValue <$>
    P.choice
      [ (<$ s) . S.Variable <$> eitherIdentifier s
      , (<$ s) . S.Literal <$> literal s
      , (<$ s) . S.Function <$> function s
      , (<$ s) . S.Application <$> application s
      , (<$ s) . S.Procedure <$> procedure s
      , (<$ s) . S.TypeAnnotation <$> typeAnnotation s
      , fmap (uncurry S.Let) <$> let' s
      , fmap (uncurry S.ForAll) <$> forAll s
      ]

procedure :: (MonadReader (Env et ev f) m, MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (NonEmpty (f (ProcedureStep et ev f))))
procedure s =
  case copoint s of
    SS.List [sk, sss] -> do
      symbol "procedure" "procedure" sk
      list1 step sss
    s' -> throwError [SexpException ("a list with 2 elements expected, but got " ++ abstractMessage s') "procedure" $ S.location s]
  where
    step :: (MonadReader (Env et ev f) m, MonadAlternativeError [Exception] m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (ProcedureStep et ev f))
    step s =
      for s $ \case
        SS.List [sk, si, sv] -> do
          symbol "bind" "procedure.step" sk
          S.BindProcedureStep <$> identifier si <*> value sv
        SS.List [sk, sv] -> do
          symbol "call" "procedure.step" sk
          S.CallProcedureStep <$> value sv
        s' -> throwError [SexpException ("a list with 2 or 3 elements expected, but got " ++ abstractMessage s') "procedure.step" $ S.location s]

let' :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (f [f (Definition et ev f)], f (Value et ev f)))
let' s =
  for s $ \case
    SS.List [sk, sds, sv] -> do
      symbol "let" "let'" sk
      (,) <$> list definition sds <*> value sv
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "let'" $ S.location s]

forAll :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (f S.Identifier, f (Value et ev f)))
forAll s =
  for s $ \case
    SS.List [sk, si, sv] -> do
      symbol "for-all" "forAll" sk
      (,) <$> identifier si <*> value sv
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "forAll" $ S.location s]

typeAnnotation :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (TypeAnnotation et ev f))
typeAnnotation s =
  for s $ \case
    SS.List [sk, sv, st] -> do
      symbol "type" "typeAnnotation" sk
      S.TypeAnnotation' <$> value sv <*> typ st
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "typeAnnotation" $ S.location s]

application :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Application et ev f))
application s =
  for s $ \case
    SS.List [sk, sv1, sv2] -> do
      symbol "apply" "application" sk
      S.ApplicationC <$> value sv1 <*> value sv2
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "application" $ S.location s]

function :: (MonadAlternativeError [Exception] m, MonadReader (Env et ev f) m, Traversable f, Copointed f, S.HasLocation f) => SexpParser f m (f (Function et ev f))
function s =
  for s $ \case
    SS.List [sk, si, st, sv] -> do
      symbol "function" "function" sk
      S.FunctionC <$> identifier si <*> typ st <*> value sv
    s' -> throwError [SexpException ("a list with 3 elements expected, but got " ++ abstractMessage s') "function" $ S.location s]

list :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f a) -> SexpParser f m (f [f a])
list p s =
  for s $ \case
    SS.List (l : ss) -> do
      symbol "list" "list" l
      traverse p ss
    s' -> throwError [SexpException ("a list with some elements and the first element \"list\" expected, but got " ++ abstractMessage s') "list" $ S.location s]

list1 :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => SexpParser f m (f a) -> SexpParser f m (f (NonEmpty (f a)))
list1 p s =
  for s $ \case
    SS.List (l : ss) -> do
      symbol "list" "list1" l
      fromMaybe X.unreachable . N.nonEmpty <$> traverse p ss
    s' -> throwError [SexpException ("a list with some elements and the first element \"list\" expected, but got " ++ abstractMessage s') "list1" $ S.location s]

atom :: (MonadError [Exception] m, Traversable f, S.HasLocation f) => String -> ParsecT Void Text Identity a -> SexpParser f m (f a)
atom label parser s =
  for s $ \case
    SS.Atom t ->
      case M.runParser (unParsecT $ parser <* P.eof) label t of
        Right i -> pure i
        Left e  -> throwError [SexpException (M.errorBundlePretty e) label $ S.location s]
    s' -> throwError [SexpException ("an atom expected, but got " ++ abstractMessage s') label $ S.location s]

symbol :: (MonadError [Exception] m, Foldable f, S.HasLocation f) => Text -> String -> SexpParser f m ()
symbol sym label s =
  for_ s $ \case
    SS.Atom sym'
      | sym == sym' -> pure ()
      | otherwise -> throwError [SexpException ("\"" ++ T.unpack sym ++ "\" expected, but got \"" ++ T.unpack sym' ++ "\"") label $ S.location s]
    s' -> throwError [SexpException ("an atom expected, but got " ++ abstractMessage s') label $ S.location s]

abstractMessage :: SS.Sexp b f -> String
abstractMessage (SS.Atom m)  = T.unpack $ "an atom of \"" <> m <> "\""
abstractMessage (SS.List ss) = "a list with " ++ show (length ss) ++ " element(s)"

data EmbeddedParser f =
  EmbeddedParser
    { embeddedValueParser :: f (Sexp f) -> Either [Exception] (f (EmbeddedValue f))
    , embeddedTypeParser  :: f (Sexp f) -> Either [Exception] (f (EmbeddedType f))
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
