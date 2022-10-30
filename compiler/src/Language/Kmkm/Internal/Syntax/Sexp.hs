{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Language.Kmkm.Internal.Syntax.Sexp
  ( Sexp (..)
  , pretty
  ) where

import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Classes        (Eq1, Ord1, Read1, Show1)
import           Data.Functor.F              (F (F), unf)
import qualified Data.Functor.F              as F
import           Data.Functor.Identity       (Identity (Identity, runIdentity))
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Exts                    (IsList (Item, fromList, toList))
import           GHC.Generics                (Generic)

data Sexp f
  = Atom Text
  | List [F f (Sexp f)]
  deriving Generic

deriving instance Show1 f => Show (Sexp f)
deriving instance Read1 f => Read (Sexp f)
deriving instance Eq1 f => Eq (Sexp f)
deriving instance Ord1 f => Ord (Sexp f)

instance FunctorB Sexp where
  bmap _ (Atom s)  = Atom s
  bmap f (List xs) = List $ F.map f . fmap (bmap f) <$> xs

instance IsString (Sexp f) where
  fromString = Atom . T.pack

instance IsList (Sexp Identity) where
  type Item (Sexp Identity) = Sexp Identity
  fromList = List . fmap (F . Identity)
  toList (Atom _) = error "toList"
  toList (List l) = fmap (runIdentity . unf) l

pretty :: Sexp Identity -> Text
pretty (Atom t)  = t
pretty (List xs) = "(" <> T.intercalate " " (map (pretty . runIdentity . unf) xs) <> ")"
