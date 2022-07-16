{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Kmkm.Internal.Syntax.Sexp
  ( Sexp (..)
  , pretty
  ) where

import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Identity       (Identity (Identity, runIdentity))
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Exts                    (IsList (Item, fromList, toList))
import           GHC.Generics                (Generic)

data Sexp f
  = Atom Text
  | List [f (Sexp f)]
  deriving Generic

deriving instance Show (f (Sexp f)) => Show (Sexp f)
deriving instance Read (f (Sexp f)) => Read (Sexp f)
deriving instance Eq (f (Sexp f)) => Eq (Sexp f)
deriving instance Ord (f (Sexp f)) => Ord (Sexp f)

instance FunctorB Sexp where
  bmap _ (Atom s)  = Atom s
  bmap f (List xs) = List $ f . fmap (bmap f) <$> xs

instance IsString (Sexp f) where
  fromString = Atom . T.pack

instance IsList (Sexp Identity) where
  type Item (Sexp Identity) = Sexp Identity
  fromList = List . fmap Identity
  toList (Atom _) = error "toList"
  toList (List l) = fmap runIdentity l

pretty :: Sexp Identity -> Text
pretty (Atom t)  = t
pretty (List xs) = "(" <> T.intercalate " " (map (pretty . runIdentity) xs) <> ")"
