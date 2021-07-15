{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds       #-}

#if __GLASGOW_HASKELL__ < 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm
  ( -- * Main interface
    compile
    -- * Exceptions
  , Exception (..)
    -- * Position
  , S.Position (..)
  ) where

import qualified Language.Kmkm.Internal.Build.NameResolve as N
import qualified Language.Kmkm.Internal.Build.TypeCheck   as T
import qualified Language.Kmkm.Internal.Compile           as C
import qualified Language.Kmkm.Internal.Exception         as X
import qualified Language.Kmkm.Internal.Parse.Sexp        as P
import qualified Language.Kmkm.Internal.Syntax            as S

import           Control.Exception.Safe (MonadCatch, catch, throw)
import qualified Control.Exception.Safe as E
import           Data.Set               (Set)
import qualified Data.Set               as H
import           Data.Text              (Text)

compile
  :: MonadCatch m
  => (FilePath -> m Text) -- ^ File reader.
  -> (FilePath -> Text -> m ()) -- ^ File writer.
  -> (Text -> m ()) -- ^ Logger.
  -> FilePath -- ^ Source file path.
  -> m ()
compile readFile writeFile writeLog src = do
  catch
    (C.compile readFile writeFile writeLog src)
    (throw . convertException)

data Exception
  = -- | An exception on parsing.
    ParseException { message :: String }
  | -- | An identifier is not found while name resolving.
    NameResolveUnknownIdentifierException { identifier :: Text, range :: Maybe (S.Position, S.Position) }
  | -- | A type is not found while type checking.
    TypeCheckNotFoundException { identifier :: Text, range :: Maybe (S.Position, S.Position) }
  | -- | Some types are mismatched while type checking.
    TypeCheckMismatchException { expected :: Text, actual :: Text, range :: Maybe (S.Position, S.Position) }
  | -- | A last procedure step is binding one while type checking.
    TypeCheckBindProcedureEndException { range :: Maybe (S.Position, S.Position) }
  | -- | A recursion is found but they have no type annotations while type checking.
    TypeCheckRecursionException { identifiers :: Set Text }
  | -- | A literal is used but its type is not imported while type checking.
    TypeCheckPrimitiveTypeException { identifier :: Text, range :: Maybe (S.Position, S.Position) }
  deriving (Show, Read, Eq, Ord)

instance E.Exception Exception

convertException :: X.Exception -> Exception
convertException e =
  case (cast e, cast e, cast e) of
    (Just (P.Exception m), Nothing, Nothing) -> ParseException m
    (Nothing, Just (N.UnknownIdentifierException i r), Nothing) -> NameResolveUnknownIdentifierException (S.pretty i) r
    (Nothing, Nothing, Just (T.NotFoundException i r)) -> TypeCheckNotFoundException (S.pretty i) r
    (Nothing, Nothing, Just (T.MismatchException e a r)) -> TypeCheckMismatchException (either id S.pretty e) (S.pretty a) r
    (Nothing, Nothing, Just (T.BindProcedureEndException r)) -> TypeCheckBindProcedureEndException r
    (Nothing, Nothing, Just (T.RecursionException is)) -> TypeCheckRecursionException $ H.map S.pretty is
    (Nothing, Nothing, Just (T.PrimitiveTypeException i r)) -> TypeCheckPrimitiveTypeException (S.pretty i) r
    _ -> X.unreachable
  where
    cast :: (E.Exception e1, E.Exception e2) => e1 -> Maybe e2
    cast = E.fromException . E.toException
