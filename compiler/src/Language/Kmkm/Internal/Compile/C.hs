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
import qualified Language.Kmkm.Internal.Build.C.Header        as KBCH
import qualified Language.Kmkm.Internal.Build.C.IntermediateC as KBCI
import qualified Language.Kmkm.Internal.Build.C.Simplify      as KBCS
import qualified Language.Kmkm.Internal.Build.C.Source        as KBCR
import qualified Language.Kmkm.Internal.Build.C.Syntax        as KBCY
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
import qualified Data.List              as L
import qualified Data.List.NonEmpty     as N
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Typeable          as Y
import           GHC.Generics           (Generic)
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
    lastStep ms fs = do
      let ms' = S.map KBCT.thunk ms
      let definedVariables = M.unions $ S.map KBCI.definedVariables ms'
      docs <- sequence $ build2_ definedVariables <$> S.toList ms'
      sequence_ $ write <$> docs
      where
        write (k, (c, h)) = do
          let path = fs M.! k
          writeFile (F.addExtension path "c") c
          writeFile (F.addExtension path "h") h
        build2_ definedVariables m =
          let
            KS.Module n _ _ = copoint m
          in do
            ds <- build2 writeLog definedVariables fs m
            pure (copoint n, ds)

build2
  :: ( MonadThrow m
     , Functor f
     , Foldable f
     , Copointed f
     , KS.HasLocation f
     )
  => (Text -> m ())
  -> Map KS.QualifiedIdentifier (KBCY.QualifiedType, [KBCY.Deriver])
  -> Map KS.ModuleName FilePath
  -> f (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed KS.EmbeddedCType KS.EmbeddedCValue B.Covered f)
  -> m (Text, Text)
build2 writeLog definedVariables fs m = do
  let KS.Module n ms _ = copoint m
  (c, h) <- build2' writeLog definedVariables m
  let
    n'@(KS.ModuleName i) = copoint n
    ms' = copoint <$> copoint ms
    key = T.toUpper (T.intercalate "_" $ N.toList i) <> "_H"
    newline :: Text
    newline = "\n"
    dir = F.takeDirectory $ fs M.! n'
    include h = "#include \"" <> T.pack (makeRelativePath dir h) <> "\"\n"
  pure
    ( fold $
        (include . flip F.addExtension "h" . (fs M.!) <$> n' : ms') ++
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
        (include . flip F.addExtension "h" . (fs M.!) <$> ms') ++
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
  -> Map KS.QualifiedIdentifier (KBCY.QualifiedType, [KBCY.Deriver])
  -> f (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed KS.EmbeddedCType KS.EmbeddedCValue B.Covered f)
  -> m (Text, Text)
build2' writeLog definedVariables m6 = do
  m7 <- KBCI.translate definedVariables m6
  writeLog $ "abstract C file: " <> T.pack (show m7)
  let c = KBCS.simplify m7
  writeLog $ "simplified abstract C file: " <> T.pack (show c)
  pure (KBCC.render $ KBCR.source c, KBCC.render $ KBCH.header c)

makeRelativePath :: FilePath -> FilePath -> FilePath
makeRelativePath base path =
  let
    baseDirs =
      let ds = F.splitDirectories base
      in if ds == ["."] then [] else ds
    pathDirs = F.splitDirectories path
    commonLength [] _ = 0
    commonLength _ [] = 0
    commonLength (a:as) (b:bs) | a == b = 1 + commonLength as bs
                               | otherwise = 0
    bl = length baseDirs
    cl = commonLength baseDirs pathDirs
  in
    if bl == cl
      then L.intercalate "/" $ drop cl pathDirs
      else if bl > cl
        then L.intercalate "/" $ replicate (bl - cl) "../" ++ drop cl pathDirs
        else KE.unreachable

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
