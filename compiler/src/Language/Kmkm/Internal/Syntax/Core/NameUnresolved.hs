module Language.Kmkm.Internal.Syntax.Core.NameUnresolved
  ( -- * Identifiers
    BindIdentifier
  , ReferenceIdentifier
  ) where

import Language.Kmkm.Internal.Syntax.Core.Common (EitherIdentifier, Identifier)

type BindIdentifier = Identifier

type ReferenceIdentifier = EitherIdentifier
