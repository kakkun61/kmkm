{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

module Data.Functor.F
  ( F(F)
  , unf
  , map
  , map2
  ) where

import Data.Copointed       (Copointed (copoint))
import Data.Function        (on)
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Read1 (liftReadPrec), Show1 (liftShowsPrec))
import Data.Kind            (Type)
import Data.Pointed         (Pointed (point))
import Data.String          (IsString (fromString))
import GHC.Exts             (IsList (Item, fromList, toList))
import GHC.Generics         (Generic)
import Prelude              (Applicative (pure), Eq ((==)), Foldable (foldr), Functor (fmap),
                             Num (abs, fromInteger, negate, signum, (*), (+)), Ord (compare, (>)), Show (showsPrec),
                             Traversable (traverse), showParen, showString, shows, undefined, ($), (.), (<$>))
import Text.Read            (Lexeme (Ident), Read (readPrec), lexP, parens, prec, step)

type F :: (Type -> Type) -> Type -> Type
newtype F f a =
  F (f a)
  deriving Generic

unf :: F f a -> f a
unf (F f) = f

instance (Show1 f, Show a) => Show (F f a) where
  showsPrec d (F f) =
    showParen (d > 10) $
      showString "F " . liftShowsPrec showsPrec shows d f

instance (Read1 f, Read a) => Read (F f a) where
  readPrec =
    parens $
      prec 10 $ do
        Ident "F" <- lexP
        f <- step $ liftReadPrec readPrec readPrec
        pure $ F f

instance (Eq1 f, Eq a) => Eq (F f a) where
  (==) = liftEq (==) `on` unf

instance (Ord1 f, Ord a) => Ord (F f a) where
  compare = liftCompare compare `on` unf

instance Copointed f => Copointed (F f) where
  copoint = copoint . unf

instance Pointed f => Pointed (F f) where
  point = F . point

instance Functor f => Functor (F f) where
  fmap f = map $ fmap f

instance Foldable f => Foldable (F f) where
  foldr f acc = foldr f acc . unf

instance Traversable f => Traversable (F f) where
  traverse f (F a) = F <$> traverse f a

instance IsString (f a) => IsString (F f a) where
  fromString = F . fromString

instance IsList (f a) => IsList (F f a) where
  type Item (F f a) = Item (f a)
  fromList = F . fromList
  toList = toList . unf

instance Num (f a) => Num (F f a) where
  (+) = map2 (+)
  (*) = map2 (*)
  abs = map abs
  signum = map signum
  fromInteger = F . fromInteger
  negate = map negate

map :: (f a -> g b) -> F f a -> F g b
map f a = F $ f $ unf a

map2 :: (f a -> g b -> h c) -> F f a -> F g b -> F h c
map2 f (F a) (F b) = F $ f a b
