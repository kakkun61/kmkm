{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

#if __GLASGOW_HASKELL__ <= 902
{-# OPTIONS_GHC -Wno-partial-fields #-}
#endif

module Language.Kmkm.Internal.Syntax.Core.Common
  ( -- * Identifiers
    Identifier (..)
  , QualifiedIdentifier (..)
  , EitherIdentifier (..)
  , ModuleName (..)
    -- * Values
  , Literal (..)
    -- * Embedded
  , Target (..)
  , EmbeddedValue (..)
  , EmbeddedCValue (..)
  , isEmbeddedCValue
  , EmbeddedType (..)
  , EmbeddedCType (..)
  , isEmbeddedCType
    -- * Locations
  , Location (..)
  , Position (..)
    -- * Higher kinded data
  , toIdentity
    -- * Pretty printing
  , Pretty (..)
  , Pretty1 (..)
  , prettyList
    -- * Utility constraint
  , Constraint
  ) where

import           Control.Monad.Identity           (Identity (Identity))
import qualified Data.Char                        as C
import           Data.Copointed                   (Copointed (copoint))
import           Data.Foldable                    (Foldable (fold))
import           Data.Functor.Barbie.Layered      (FunctorB (bmap))
import           Data.Functor.Classes             (Eq1, Ord1, Read1, Show1)
import           Data.Functor.F                   (F (F))
import qualified Data.Functor.F                   as F
import           Data.Functor.With                (With, pattern With)
import qualified Data.Kind                        as K
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty               as N
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Exts                         (IsList (Item, fromList, toList), IsString (fromString))
import qualified GHC.Exts                         as E
import           GHC.Generics                     (Generic)
import qualified Language.Kmkm.Internal.Exception as X
import           Text.Show.Unicode                (ushow)

-- Identifiers

-- | An identifier.
data Identifier
  = UserIdentifier Text
  | SystemIdentifier Char Word
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = UserIdentifier . fromString

instance Pretty Identifier where
  pretty (UserIdentifier t)     = t
  pretty (SystemIdentifier c n) = "_" <> T.pack [c] <> T.pack (show n)

-- | A qualified identifier.
data QualifiedIdentifier
  = GlobalIdentifier ModuleName Identifier
  | LocalIdentifier Identifier
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString QualifiedIdentifier where
  fromString = LocalIdentifier . fromString

-- | Note that these are partial functions.
instance IsList QualifiedIdentifier where
  type Item QualifiedIdentifier = Text

  fromList [] = error "more than 1 items necessary"
  fromList [t] = LocalIdentifier $ UserIdentifier t
  fromList ts =
    let
      n = ModuleName $ N.fromList $ init ts
      i = UserIdentifier $ last ts
    in GlobalIdentifier n i

  toList (GlobalIdentifier (ModuleName n) (UserIdentifier t)) = N.toList n ++ [t]
  toList (LocalIdentifier (UserIdentifier t))                 = [t]
  toList _                                                    = error "system identifiers are not acceptable"

instance Pretty QualifiedIdentifier where
  pretty (GlobalIdentifier n i) = pretty n <> "." <> pretty i
  pretty (LocalIdentifier i)    = pretty i

data EitherIdentifier
  = UnqualifiedIdentifier Identifier
  | QualifiedIdentifier QualifiedIdentifier
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString EitherIdentifier where
  fromString = UnqualifiedIdentifier . fromString

-- | Note that these are partial functions.
instance IsList EitherIdentifier where
  type Item EitherIdentifier = Text

  fromList []  = error "more than 1 items necessary"
  fromList [t] = UnqualifiedIdentifier $ UserIdentifier t
  fromList ts  = QualifiedIdentifier $ E.fromList ts

  toList (QualifiedIdentifier i)                    = E.toList i
  toList (UnqualifiedIdentifier (UserIdentifier t)) = [t]
  toList _                                          = error "system identifiers are not acceptable"

instance Pretty EitherIdentifier where
  pretty (UnqualifiedIdentifier i) = pretty i
  pretty (QualifiedIdentifier i)   = pretty i

-- ModuleName

-- | A module name.
newtype ModuleName = ModuleName (N.NonEmpty Text) deriving (Show, Read, Eq, Ord, Generic)

instance IsString ModuleName where
  fromString = ModuleName . (:| []) . fromString

instance IsList ModuleName where
  type Item ModuleName = Text
  fromList = ModuleName . N.fromList
  toList (ModuleName n) = N.toList n

instance Pretty ModuleName where
  pretty (ModuleName ts) = fold $ N.intersperse "." ts

-- Literal

data Literal
  = Integer { value :: Integer, base :: Word }
  | Fraction { significand :: Integer, fractionDigits :: Word, exponent :: Int, base :: Word }
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Num Literal where
  fromInteger v = Integer v 10
  (+) = X.unreachable "(+)"
  (*) = X.unreachable "(*)"
  abs = X.unreachable "abs"
  signum = X.unreachable "signum"
  negate = X.unreachable "negate"

instance IsString Literal where
  fromString = String . fromString

instance Pretty Literal where
  pretty (Integer v 2)  = "0b" <> integerToText 2 v
  pretty (Integer v 8)  = "0o" <> integerToText 8 v
  pretty (Integer v 10) = integerToText 10 v
  pretty (Integer v 16) = "0x" <> integerToText 16 v
  pretty (Integer _ b)  = X.unreachable $ "base " ++ show b ++ " is not supported"
  pretty v@Fraction {}  = T.pack $ show v
  pretty (String v)     = T.pack $ ushow v

integerToText :: Word -> Integer -> Text
integerToText b v =
  T.pack $ go v []
  where
    go 0 [] = "0"
    go 0 acc = acc
    go v acc =
      let
        (p, q) = v `divMod` fromIntegral b
        q' = fromIntegral q
        acc' =
          if q < 10
            then C.chr (q' + C.ord '0') : acc
            else C.chr (q' + C.ord 'A') : acc
      in go p acc'

-- Target

data Target
  = C
  deriving (Show, Read, Eq, Ord, Enum, Generic)

-- EmbeddedValue

newtype EmbeddedValue f
  = EmbeddedValueC (EmbeddedCValue f)
  deriving Generic

deriving instance Show1 f => Show (EmbeddedValue f)
deriving instance Read1 f => Read (EmbeddedValue f)
deriving instance Eq1 f => Eq (EmbeddedValue f)
deriving instance Ord1 f => Ord (EmbeddedValue f)

instance FunctorB EmbeddedValue where
  bmap f (EmbeddedValueC v) = EmbeddedValueC (bmap f v)

instance Pretty1 f => Pretty (EmbeddedValue f) where
  pretty (EmbeddedValueC v) = pretty v

-- EmbeddedCValue

data EmbeddedCValue f =
  EmbeddedCValue { include :: F f Text, parameters :: F f [F f Text], body :: F f Text }
  deriving Generic

deriving instance Show1 f => Show (EmbeddedCValue f)
deriving instance Read1 f => Read (EmbeddedCValue f)
deriving instance Eq1 f => Eq (EmbeddedCValue f)
deriving instance Ord1 f => Ord (EmbeddedCValue f)

instance FunctorB EmbeddedCValue where
  bmap f (EmbeddedCValue i ps b) = EmbeddedCValue (F.map f i) (fmap (F.map f) <$> F.map f ps) (F.map f b)

isEmbeddedCValue :: Traversable f => F f (EmbeddedValue f) -> Maybe (F f (EmbeddedCValue f))
isEmbeddedCValue = traverse $ \(EmbeddedValueC v) -> Just v

instance Pretty1 f => Pretty (EmbeddedCValue f) where
  pretty (EmbeddedCValue i ps b) = prettyList "c-value" [pretty i, pretty ps, pretty b]

-- EmbeddedType

newtype EmbeddedType f
  = EmbeddedTypeC (EmbeddedCType f)
  deriving Generic

deriving instance Show1 f => Show (EmbeddedType f)
deriving instance Read1 f => Read (EmbeddedType f)
deriving instance Eq1 f => Eq (EmbeddedType f)
deriving instance Ord1 f => Ord (EmbeddedType f)

instance FunctorB EmbeddedType where
  bmap f (EmbeddedTypeC t) = EmbeddedTypeC (bmap f t)

instance Pretty1 f => Pretty (EmbeddedType f) where
  pretty (EmbeddedTypeC t) = pretty t

-- EmbeddedCType

data EmbeddedCType f =
  EmbeddedCType { include :: F f Text, body :: F f Text }
  deriving Generic

deriving instance Show1 f => Show (EmbeddedCType f)
deriving instance Read1 f => Read (EmbeddedCType f)
deriving instance Eq1 f => Eq (EmbeddedCType f)
deriving instance Ord1 f => Ord (EmbeddedCType f)

instance FunctorB EmbeddedCType where
  bmap f (EmbeddedCType i b) = EmbeddedCType (F.map f i) (F.map f b)

isEmbeddedCType :: Traversable f => F f (EmbeddedType f) -> Maybe (F f (EmbeddedCType f))
isEmbeddedCType = traverse $ \(EmbeddedTypeC v) -> Just v

instance Pretty1 f => Pretty (EmbeddedCType f) where
  pretty (EmbeddedCType i b) = prettyList "c-type" [pretty i, pretty b]

-- Locations

data Location =
  Location
    { filePath :: FilePath
    , begin    :: Position
    , end      :: Position
    }
  deriving (Show, Read, Eq, Ord, Generic)

-- | A position in a source file.
data Position =
  Position { line :: Word, column :: Word }
  deriving (Show, Read, Eq, Ord, Generic)

instance Pretty a => Pretty (With Location a) where
  pretty (With _ a) = pretty a

-- Higher kinded data

toIdentity :: (Functor f, Copointed f, FunctorB b) => f (b f) -> Identity (b Identity)
toIdentity = Identity . bmap (Identity . copoint) . copoint

-- Pretty

-- | A pretty-printable class.
class Pretty a where
  pretty :: a -> Text

class Pretty1 f where
  liftPretty :: (a -> Text) -> f a -> Text

instance Pretty Text where
  pretty = T.pack . show

instance (Pretty1 f, Pretty a) => Pretty (f a) where
  pretty = liftPretty pretty

instance Pretty1 Identity where
  liftPretty p (Identity a) = p a

instance Pretty1 [] where
  liftPretty p as = prettyList "list" $ p <$> as

instance Pretty1 NonEmpty where
  liftPretty p as = prettyList "list" $ N.toList $ p <$> as

instance Pretty a => Pretty1 ((,) a) where
  liftPretty p (a, b) = "(" <> pretty a <> ", " <> p b <> ")"

instance Pretty1 f => Pretty1 (F f) where
  liftPretty p (F f) = liftPretty p f

prettyList :: Text -> [Text] -> Text
prettyList s ss = "(" <> s <> foldMap (" " <>) ss <> ")"

type Constraint :: (K.Type -> K.Constraint) -> ((K.Type -> K.Type) -> K.Constraint) -> ((K.Type -> K.Type) -> K.Type) -> ((K.Type -> K.Type) -> K.Type) -> (K.Type -> K.Type) -> K.Constraint
type Constraint cls cls1 et ev f =
  ( cls1 f
  , cls (et f)
  , cls (ev f)
  )
