{-# LANGUAGE PatternSynonyms #-}

module Language.Kmkm
  ( compile
  , S.Position (..)
  , S.Pretty (..)
  , Exception (..)
  , ParseException
  , pattern ParseException
  , TypeCheckException
  , pattern TypeCheckNotFoundException
  , pattern TypeCheckMismatchException
  , pattern TypeCheckBindProcedureEndException
  , pattern TypeCheckRecursionException
  , NameResolveException
  , pattern NameResolveUnknownIdentifierException
  ) where

import           Data.Set                        (Set)
import qualified Language.Kmkm.Build.NameResolve as BN
import qualified Language.Kmkm.Build.TypeCheck   as BT
import           Language.Kmkm.Compile           (compile)
import           Language.Kmkm.Exception         (Exception (Exception))
import qualified Language.Kmkm.Parse.Sexp        as P
import qualified Language.Kmkm.Syntax            as S

type ParseException = P.Exception

pattern ParseException :: String -> ParseException
pattern ParseException m = P.Exception m

{-# COMPLETE ParseException #-}

type TypeCheckException = BT.Exception

pattern TypeCheckNotFoundException :: S.QualifiedIdentifier -> Maybe (S.Position, S.Position) -> TypeCheckException
pattern TypeCheckNotFoundException i r = BT.NotFoundException i r

pattern TypeCheckMismatchException :: String -> String -> TypeCheckException
pattern TypeCheckMismatchException expected actual = BT.MismatchException expected actual

pattern TypeCheckBindProcedureEndException :: Maybe (S.Position, S.Position) -> TypeCheckException
pattern TypeCheckBindProcedureEndException r = BT.BindProcedureEndException r

pattern TypeCheckRecursionException :: Set S.QualifiedIdentifier -> TypeCheckException
pattern TypeCheckRecursionException is = BT.RecursionException is

{-# COMPLETE TypeCheckNotFoundException, TypeCheckMismatchException, TypeCheckBindProcedureEndException, TypeCheckRecursionException #-}

type NameResolveException = BN.Exception

pattern NameResolveUnknownIdentifierException :: S.EitherIdentifier -> Maybe (S.Position, S.Position) -> NameResolveException
pattern NameResolveUnknownIdentifierException i r = BN.UnknownIdentifierException i r

{-# COMPLETE NameResolveUnknownIdentifierException #-}
