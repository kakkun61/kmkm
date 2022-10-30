{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Kmkm.Internal.Compile.C
  ( compile
  ) where

import qualified Language.Kmkm.Internal.Build.C.C                   as KBCC
import qualified Language.Kmkm.Internal.Build.C.Header              as KBCH
import qualified Language.Kmkm.Internal.Build.C.IntermediateC       as KBCI
import qualified Language.Kmkm.Internal.Build.C.PolymorphicPoint    as KBCP
import qualified Language.Kmkm.Internal.Build.C.Simplify            as KBCS
import qualified Language.Kmkm.Internal.Build.C.Source              as KBCR
import qualified Language.Kmkm.Internal.Build.C.Syntax              as KBCY
import qualified Language.Kmkm.Internal.Build.C.Thunk               as KBCT
import qualified Language.Kmkm.Internal.Compile                     as KC
import qualified Language.Kmkm.Internal.Exception                   as KE
import qualified Language.Kmkm.Internal.Parse.Sexp.C                as KPC
import qualified Language.Kmkm.Internal.Syntax.C.PolymorphicPointed as KS6
import qualified Language.Kmkm.Internal.Syntax.Core.Common          as KSC

import           Control.Applicative    (Alternative)
import           Control.Exception.Safe (MonadCatch, MonadThrow)
import           Data.Copointed         (Copointed (copoint))
import           Data.Foldable          (Foldable (fold))
import           Data.Functor.F         (F)
import           Data.Functor.With      (With)
import qualified Data.List              as L
import qualified Data.List.NonEmpty     as N
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           GHC.Stack              (HasCallStack)
import qualified System.FilePath        as F

type WL = With KSC.Location

compile
  :: (Alternative m, MonadCatch m, HasCallStack)
  => (FilePath -> m FilePath) -- ^ File finder.
  -> (FilePath -> m Text) -- ^ File reader.
  -> (FilePath -> Text -> m ()) -- ^ File writer.
  -> (Text -> m ()) -- ^ Logger.
  -> FilePath -- ^ Source file path.
  -> m ()
compile findFile readFile writeFile writeLog src =
  KC.compile [KPC.embeddedParser] KSC.isEmbeddedCType KSC.isEmbeddedCValue lastStep findFile readFile writeLog src
  where
    lastStep ms fs = do
      let
        ms' = KBCT.thunk <$> ms
        variablePolymorphicPoints = M.unions $ KBCP.variablePolymorphicPoints <$> ms'
      ms'' <- traverse (KBCP.attach variablePolymorphicPoints) ms'
      let definedVariables = M.unions $ KBCI.definedVariables <$> ms''
      docs <-
        for ms'' $ \m -> do
          let KS6.Module n _ _ = copoint m
          ds <- build2 writeLog definedVariables fs m
          pure (copoint n, ds)
      mapM_ write docs
      where
        write (k, (c, h)) = do
          let path = fs M.! k
          writeFile (F.addExtension path "c") c
          writeFile (F.addExtension path "h") h

build2
  :: (MonadThrow m, HasCallStack)
  => (Text -> m ())
  -> Map KSC.QualifiedIdentifier (KBCY.QualifiedType, [KBCY.Deriver])
  -> Map KSC.ModuleName FilePath
  -> F WL (KS6.Module KSC.EmbeddedCType KSC.EmbeddedCValue WL)
  -> m (Text, Text)
build2 writeLog definedVariables fs m = do
  let KS6.Module n ms _ = copoint m
  (c, h) <- build2' writeLog definedVariables m
  let
    n'@(KSC.ModuleName i) = copoint n
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
  :: (MonadThrow m, HasCallStack)
  => (Text -> m ())
  -> Map KSC.QualifiedIdentifier (KBCY.QualifiedType, [KBCY.Deriver])
  -> F WL (KS6.Module KSC.EmbeddedCType KSC.EmbeddedCValue WL)
  -> m (Text, Text)
build2' writeLog definedVariables m6 = do
  m7 <- KBCI.translate definedVariables m6
  writeLog $ "abstract C file: " <> T.pack (show m7)
  let c = KBCS.simplify m7
  writeLog $ "simplified abstract C file: " <> T.pack (show c)
  pure (KBCC.render $ KBCR.source c, KBCC.render $ KBCH.header c)

makeRelativePath :: HasCallStack => FilePath -> FilePath -> FilePath
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
        else KE.unreachable "length of base ≦ length of common"
