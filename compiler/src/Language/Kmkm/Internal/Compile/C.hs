{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Kmkm.Internal.Compile.C
  ( compile
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Build.C.C             as KBCC
import qualified Language.Kmkm.Internal.Build.C.Declare       as KBCD
import qualified Language.Kmkm.Internal.Build.C.IntermediateC as KBCI
import qualified Language.Kmkm.Internal.Build.C.Simplify      as KBCS
import qualified Language.Kmkm.Internal.Build.C.Thunk         as KBCT
import qualified Language.Kmkm.Internal.Compile               as KC
import qualified Language.Kmkm.Internal.Exception             as KE
import qualified Language.Kmkm.Internal.Parse.Sexp.C          as KPC
import qualified Language.Kmkm.Internal.Syntax                as KS

import qualified Barbies.Bare           as B
import qualified Control.Exception      as E
import           Control.Exception.Safe (MonadCatch, MonadThrow, throw)
import           Data.Copointed         (Copointed (copoint))
import           Data.Foldable          (Foldable (fold))
import qualified Data.List.NonEmpty     as N
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Typeable          as Y
import           GHC.Generics           (Generic)
import           System.FilePath        (pathSeparator)
import qualified System.FilePath        as F

compile
  :: MonadCatch m
  => (FilePath -> m FilePath) -- ^ File finder.
  -> (FilePath -> m Text) -- ^ File reader.
  -> (FilePath -> Text -> m ()) -- ^ File writer.
  -> (Text -> m ()) -- ^ Logger.
  -> FilePath -- ^ Source file path.
  -> m ()
compile _ _ _ _ src@('.' : '.' : _) = throw $ DotDotPathException src
compile findFile readFile writeFile writeLog src =
  KC.compile [KPC.embeddedParser] KS.isEmbeddedCType KS.isEmbeddedCValue lastStep findFile readFile writeLog src
  where
    lastStep ms = do
      let ms' = KBCT.thunk <$> ms
      let typeOrigins = M.unions $ KBCI.typeOrigins <$> ms'
      docs <- sequence $ build2_ typeOrigins <$> ms'
      sequence_ $ write <$> docs
    write (k, (c, h)) = do
      let
        path = moduleNameToFilePath k
      writeFile (F.addExtension path "c") c
      writeFile (F.addExtension path "h") h
    build2_ typeOrigins m =
      let KS.Module n _ _ = copoint m
      in do
        ds <- build2 writeLog typeOrigins m
        pure (copoint n, ds)

build2
  :: ( MonadThrow m
     , Functor f
     , Foldable f
     , Copointed f
     , KS.HasLocation f
     )
  => (Text -> m ())
  -> Map KS.QualifiedIdentifier KBCI.TypeOrigin
  -> f (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed KS.EmbeddedCType KS.EmbeddedCValue B.Covered f)
  -> m (Text, Text)
build2 writeLog typeOrigins m = do
  let KS.Module n ms _ = copoint m
  (c, h) <- build2' writeLog typeOrigins m
  let
    n'@(KS.ModuleName i) = copoint n
    ms' = copoint <$> copoint ms
    key = T.toUpper (T.intercalate "_" $ N.toList i) <> "_H"
    newline :: Text
    newline = "\n"
    include h  = "#include \"" <> h <> "\"\n"
  pure
    ( fold $
        (include . moduleNameToHeaderPath <$> n' : ms') ++
        [ newline
        , c
        , newline
        ]
    , fold $
        [ "#ifndef "
        , key
        , newline
        , "#define "
        , key
        , newline
        ] ++
        (include . moduleNameToHeaderPath <$> ms') ++
        [ h
        , newline
        , "#endif"
        , newline
        ]
    )

build2'
  :: ( MonadThrow m
     , Functor f
     , Foldable f
     , Copointed f
     , KS.HasLocation f
     )
  => (Text -> m ())
  -> Map KS.QualifiedIdentifier KBCI.TypeOrigin
  -> f (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed KS.EmbeddedCType KS.EmbeddedCValue B.Covered f)
  -> m (Text, Text)
build2' writeLog typeOrigins m6 = do
  m7 <- KBCI.translate typeOrigins m6
  writeLog $ "abstract C file: " <> T.pack (show m7)
  let c = KBCS.simplify m7
  writeLog $ "simplified abstract C file: " <> T.pack (show c)
  pure (KBCC.render c, KBCC.render $ KBCD.declare c)

moduleNameToFilePath :: KS.ModuleName -> FilePath
moduleNameToFilePath (KS.ModuleName n) = T.unpack $ T.intercalate (T.singleton pathSeparator) $ N.toList n

moduleNameToHeaderPath :: KS.ModuleName -> Text
moduleNameToHeaderPath (KS.ModuleName n) = T.intercalate "/" (N.toList n) <> ".h"

data Exception
  = RecursionException (N.NonEmpty KS.ModuleName)
  | ModuleNameMismatchException FilePath KS.ModuleName (Maybe KS.Location)
  | DotDotPathException FilePath
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . KE.Exception
  fromException e = do
    KE.Exception e <- E.fromException e
    Y.cast e
