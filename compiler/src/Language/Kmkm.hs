{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

#if __GLASGOW_HASKELL__ < 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm
  ( -- * Main interface
    compile
    -- * Exceptions
  , Exception (..)
    -- * Locations
  , S.Location (..)
  , S.Position (..)
  ) where

import qualified Language.Kmkm.Internal.Build.C.IntermediateC as I
import qualified Language.Kmkm.Internal.Build.NameResolve     as N
import qualified Language.Kmkm.Internal.Build.TypeCheck       as T
import qualified Language.Kmkm.Internal.Compile.C             as C
import qualified Language.Kmkm.Internal.Exception             as X
import qualified Language.Kmkm.Internal.Parse.Sexp            as P
import qualified Language.Kmkm.Internal.Syntax                as S

import           Control.Exception.Safe (MonadCatch, catch, throw)
import qualified Control.Exception.Safe as E
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Set               (Set)
import qualified Data.Set               as H
import           Data.Text              (Text)

compile
  :: MonadCatch m
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
    ParseException String -- ^ message.
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
      (Maybe S.Location) -- ^ location .
  | -- | A number of parameters of embedded C is different from one of its type while intermediate C translating.
    IntermediateCEmbeddedParameterMismatchException
      Word -- ^ expected
      Word -- ^ actual
      (Maybe S.Location) -- ^ location.
  | -- | Modules' dependency have a recursion while compiling.
    CompileRecursionException
      (NonEmpty Text) -- ^ modules.
  | -- | A file's name and its enclosed module's name is mismatched while compiling.
    CompileModuleNameMismatchException
      FilePath -- ^ file name.
      Text -- ^ module name.
      (Maybe S.Location) -- ^ location.
  | -- | A file's path has ".." while compiling.
    CompileDotDotPathException
      FilePath -- ^ file path
  deriving (Show, Read, Eq, Ord)

instance E.Exception Exception

convertException :: X.Exception -> Exception
convertException e =
  case (cast e, cast e, cast e, cast e, cast e) of
    (Just (P.Exception m), Nothing, Nothing, Nothing, Nothing) -> ParseException m
    (Nothing, Just (N.UnknownIdentifierException i r), Nothing, Nothing, Nothing) -> NameResolveUnknownIdentifierException (S.pretty i) r
    (Nothing, Nothing, Just (T.NotFoundException i r), Nothing, Nothing) -> TypeCheckNotFoundException (S.pretty i) r
    (Nothing, Nothing, Just (T.MismatchException e a r), Nothing, Nothing) -> TypeCheckMismatchException (either id S.pretty e) (S.pretty a) r
    (Nothing, Nothing, Just (T.BindProcedureEndException r), Nothing, Nothing) -> TypeCheckBindProcedureEndException r
    (Nothing, Nothing, Just (T.RecursionException is), Nothing, Nothing) -> TypeCheckRecursionException $ H.map S.pretty is
    (Nothing, Nothing, Just (T.PrimitiveTypeException i r), Nothing, Nothing) -> TypeCheckPrimitiveTypeException (S.pretty i) r
    (Nothing, Nothing, Nothing, Just (I.EmbeddedParameterMismatchException e a r), Nothing) -> IntermediateCEmbeddedParameterMismatchException e a r
    (Nothing, Nothing, Nothing, Nothing, Just (C.RecursionException ms)) -> CompileRecursionException $ S.pretty <$> ms
    (Nothing, Nothing, Nothing, Nothing, Just (C.ModuleNameMismatchException f m r)) -> CompileModuleNameMismatchException f (S.pretty m) r
    (Nothing, Nothing, Nothing, Nothing, Just (C.DotDotPathException f)) -> CompileDotDotPathException f
    _ -> X.unreachable
  where
    cast :: (E.Exception e1, E.Exception e2) => e1 -> Maybe e2
    cast = E.fromException . E.toException
