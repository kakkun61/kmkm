{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Language.Kmkm.Exception
  ( Exception (..)
  , unreachable
  ) where

import qualified Control.Exception as E
import           GHC.Stack         (HasCallStack)

-- | A root exception.
data Exception = forall e. E.Exception e => Exception e

deriving instance Show Exception

instance E.Exception Exception

unreachable :: HasCallStack => a
unreachable = error "compiler bug"
