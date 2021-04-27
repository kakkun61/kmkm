{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Language.Kmkm.Exception
  ( Exception (..)
  ) where

import qualified Control.Exception as E

data Exception = forall e. E.Exception e => Exception e

deriving instance Show Exception

instance E.Exception Exception
