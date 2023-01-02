{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}

#if __GLASGOW_HASKELL__ <= 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm
  ( -- * Main interface
    compile
    -- * Exceptions
  , Exception (..)
  , ParseExceptionMessage (..)
    -- * Locations
  , S.Location (..)
  , S.Position (..)
    --
  ) where

import qualified Language.Kmkm.Internal.Build.C.IntermediateC    as I
import qualified Language.Kmkm.Internal.Build.C.PolymorphicPoint as O
import qualified Language.Kmkm.Internal.Build.NameResolve        as N
import qualified Language.Kmkm.Internal.Build.TypeCheck          as T
import qualified Language.Kmkm.Internal.Compile                  as M
import qualified Language.Kmkm.Internal.Compile.C                as C
import qualified Language.Kmkm.Internal.Exception                as X
import qualified Language.Kmkm.Internal.Parse.Sexp               as P
import qualified Language.Kmkm.Internal.Syntax.Core.Common       as S

import           Control.Applicative    (Alternative)
import           Control.Exception.Safe (MonadCatch, catch, throw)
import qualified Control.Exception.Safe as E
import           Data.Foldable          (Foldable (fold))
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Monoid            (Alt (Alt, getAlt))
import           Data.Set               (Set)
import qualified Data.Set               as H
import           Data.Text              (Text)
import           GHC.Stack              (HasCallStack)

compile
  :: (Alternative m, MonadCatch m, HasCallStack)
  => (FilePath -> m FilePath) -- ^ File finder.
  -> (FilePath -> m Text) -- ^ File reader.
  -> (FilePath -> Text -> m ()) -- ^ File writer.
  -> (Text -> m ()) -- ^ Logger.
  -> FilePath -- ^ Source file path.
  -> m ()
compile findFile readFile writeFile writeLog src = do
  catch
    (C.compile findFile readFile writeFile writeLog src)
    (throw . convertException)

data Exception
  = -- | An exception on parsing.
    ParseException [ParseExceptionMessage]
  | -- | An identifier is not found while name resolving.
    NameResolveUnknownIdentifierException
      Text -- ^ identifier.
      (Maybe S.Location) -- ^ location.
  | -- | A type is not found while type checking.
    TypeCheckNotFoundException
      Text -- ^ identifier.
      (Maybe S.Location) -- ^ location.
  | -- | Some types are mismatched while type checking.
    TypeCheckMismatchException
      Text -- ^ expected.
      Text -- ^ actual.
      (Maybe S.Location) -- ^ location.
  | -- | A last procedure step is binding one while type checking.
    TypeCheckBindProcedureEndException
      (Maybe S.Location) -- ^ location.
  | -- | A recursion is found but they have no type annotations while type checking.
    TypeCheckRecursionException
      (Set Text) -- ^ identifiers.
  | -- | A literal is used but its type is not imported while type checking.
    TypeCheckPrimitiveTypeException
      Text -- ^ identifier.
      (Maybe S.Location) -- ^ location.
  | -- | A number of parameters of embedded C is different from one of its type while intermediate C translating.
    IntermediateCEmbeddedParameterMismatchException
      Word -- ^ expected
      Word -- ^ actual
      (Maybe S.Location) -- ^ location.
  | -- | A variable is not found while polymorphic parameter checking.
    PolymorphicPointNotFoundException
      Text -- ^ identifier.
      (Maybe S.Location) -- ^ location.
  | -- | Metadata is not attached while polymorphic parameter checking.
    PolymorphicPointNoMetadataException
      (Maybe S.Location) -- ^ location.
  | -- | Modules' dependency have a recursion while compiling.
    CompileRecursionException
      (NonEmpty Text) -- ^ modules.
  | -- | A file's name and its enclosed module's name are mismatched while compiling.
    CompileModuleNameMismatchException
      FilePath -- ^ file name.
      Text -- ^ module name.
      (Maybe S.Location) -- ^ location.
  | -- | A file's path has ".." while compiling.
    CompileDotDotPathException
      FilePath -- ^ file path
  deriving (Show, Read, Eq, Ord)

instance E.Exception Exception

data ParseExceptionMessage
  = ParseTextMessage String
  | ParseSexpMessage String (Maybe S.Location)
  deriving (Show, Read, Eq, Ord)

convertException :: HasCallStack => X.Exception -> Exception
convertException e =
  let
    e' =
      getAlt $
        fold
          [ flip fmap (Alt $ cast e) $
              ParseException . (convertParseExceptionMessage <$>)
          , flip fmap (Alt $ cast e) $
              \(N.UnknownIdentifierException i r) -> NameResolveUnknownIdentifierException (S.pretty i) r
          , flip fmap (Alt $ cast e) $ \case
              (T.NotFoundException i r)       -> TypeCheckNotFoundException (S.pretty i) r
              (T.MismatchException e a r)     -> TypeCheckMismatchException (either id S.pretty e) (S.pretty a) r
              (T.BindProcedureEndException r) -> TypeCheckBindProcedureEndException r
              (T.RecursionException is)       -> TypeCheckRecursionException $ H.map S.pretty is
              (T.PrimitiveTypeException i r)  -> TypeCheckPrimitiveTypeException (S.pretty i) r
          , flip fmap (Alt $ cast e) $
              \(I.EmbeddedParameterMismatchException e a r) -> IntermediateCEmbeddedParameterMismatchException e a r
          , flip fmap (Alt $ cast e) $ \case
              (O.NotFoundException i r) -> PolymorphicPointNotFoundException (S.pretty i) r
              (O.NoMetadataException r) -> PolymorphicPointNoMetadataException r
          , flip fmap (Alt $ cast e) $ \case
              (M.RecursionException ms)             -> CompileRecursionException $ S.pretty <$> ms
              (M.ModuleNameMismatchException f m r) -> CompileModuleNameMismatchException f (S.pretty m) r
              (M.DotDotPathException f)             -> CompileDotDotPathException f
          ]
  in
    case e' of
      Just e'' -> e''
      Nothing  -> X.unreachable $ "unexpected exception: " ++ E.displayException e
  where
    cast :: (E.Exception e1, E.Exception e2) => e1 -> Maybe e2
    cast = E.fromException . E.toException
    convertParseExceptionMessage (P.TextException m)     = ParseTextMessage m
    convertParseExceptionMessage (P.SexpException m p r) = ParseSexpMessage (m ++ " at " ++ p) r
