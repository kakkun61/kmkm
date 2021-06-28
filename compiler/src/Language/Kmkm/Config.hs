{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Config
  ( Config (..)
  , TypeMap (..)
  ) where

import Language.Kmkm.Syntax (CHeader)

import Data.Default.Class (Default (def))
import Data.Text          (Text)
import GHC.Generics       (Generic)

data Config =
  Config
    { headers :: [CHeader]
    , typeMap :: TypeMap
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance Default Config where
  def = Config [] def

data TypeMap =
  TypeMap
    { int   :: Text
    , uint  :: Text
    , byte  :: Text
    , frac  :: Text
    , frac2 :: Text
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance Default TypeMap where
  def =
    TypeMap
      { int = "int"
      , uint = "unsigned int"
      , byte = "uint8_t"
      , frac = "float"
      , frac2 = "double"
      }
