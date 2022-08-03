{-# LANGUAGE PatternSynonyms #-}

module Language.Kmkm.Internal.Parse.Location
  ( withLocation
  ) where

import qualified Language.Kmkm.Internal.Syntax as S

import qualified Text.Megaparsec as M
import Data.Functor.With ( With, pattern With )

withLocation :: M.MonadParsec e s p => FilePath -> p a -> p (With S.Location a)
withLocation filePath p = do
  M.SourcePos _ beginLine beginColumn <- M.getSourcePos
  a <- p
  M.SourcePos _ endLine endColumn <- M.getSourcePos
  pure $
    With
      ( S.Location
          filePath
          (S.Position (fromIntegral $ M.unPos beginLine) (fromIntegral $ M.unPos beginColumn))
          (S.Position (fromIntegral $ M.unPos endLine) (fromIntegral $ M.unPos endColumn))
      )
      a
