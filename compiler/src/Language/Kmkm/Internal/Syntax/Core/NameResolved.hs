module Language.Kmkm.Internal.Syntax.Core.NameResolved
  ( -- * Identifiers
    BindIdentifier
  , ReferenceIdentifier
  ) where

import Language.Kmkm.Internal.Syntax.Core.Common (QualifiedIdentifier)

type BindIdentifier = QualifiedIdentifier

type ReferenceIdentifier = QualifiedIdentifier
