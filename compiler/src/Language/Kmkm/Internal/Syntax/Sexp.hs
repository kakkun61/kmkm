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

import           Barbies.Bare                (Bare, Covered, Wear)
import           Barbies.Bare.Layered        (BareB (bcover, bstrip))
import           Data.Functor.Barbie.Layered (FunctorB (bmap))
import           Data.Functor.Identity       (Identity (Identity, runIdentity))
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Exts                    (IsList (Item, fromList, toList))
import           GHC.Generics                (Generic)

data Sexp b f
  = Atom Text
  | List [Wear b f (Sexp b f)]
  deriving Generic

deriving instance Show (Wear b f (Sexp b f)) => Show (Sexp b f)
deriving instance Read (Wear b f (Sexp b f)) => Read (Sexp b f)
deriving instance Eq (Wear b f (Sexp b f)) => Eq (Sexp b f)
deriving instance Ord (Wear b f (Sexp b f)) => Ord (Sexp b f)

instance FunctorB (Sexp Covered) where
  bmap _ (Atom s)  = Atom s
  bmap f (List xs) = List $ f . fmap (bmap f) <$> xs

instance BareB Sexp where
  bstrip (Atom s)  = Atom s
  bstrip (List xs) = List $ bstrip . runIdentity <$> xs

  bcover (Atom s)  = Atom s
  bcover (List xs) = List $ Identity . bcover <$> xs

instance IsString (Sexp b f) where
  fromString = Atom . T.pack

instance IsList (Sexp Covered Identity) where
  type Item (Sexp Covered Identity) = Sexp Covered Identity
  fromList = List . fmap Identity
  toList (Atom _) = error "toList"
  toList (List l) = fmap runIdentity l

instance IsList (Sexp Bare Identity) where
  type Item (Sexp Bare Identity) = Sexp Bare Identity
  fromList = List
  toList (Atom _) = error "toList "
  toList (List l) = l

pretty :: Sexp Bare Identity -> Text
pretty (Atom t)  = t
pretty (List xs) = "(" <> T.intercalate " " (map pretty xs) <> ")"
