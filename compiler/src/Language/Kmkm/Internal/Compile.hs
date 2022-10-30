{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Internal.Compile
  ( compile
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Build.LambdaLift                                          as KBL
import qualified Language.Kmkm.Internal.Build.NameResolve                                         as KBN
import qualified Language.Kmkm.Internal.Build.PartiallyApply                                      as KBP
import qualified Language.Kmkm.Internal.Build.TypeCheck                                           as KBT
import qualified Language.Kmkm.Internal.Build.Uncurry                                             as KBU
import qualified Language.Kmkm.Internal.Exception                                                 as KE
import qualified Language.Kmkm.Internal.Exception                                                 as X
import qualified Language.Kmkm.Internal.Parse.Sexp                                                as KP
import qualified Language.Kmkm.Internal.Syntax.Core.Common                                        as KSC
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Curried                          as KST1
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Curried.LambdaUnlifted     as KS3
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Typed.Uncurried.LambdaLifted     as KS5
import qualified Language.Kmkm.Internal.Syntax.Core.NameResolved.Untyped.Curried.LambdaUnlifted   as KS2
import qualified Language.Kmkm.Internal.Syntax.Core.NameUnresolved.Untyped.Curried.LambdaUnlifted as KS1

import qualified Algebra.Graph.AdjacencyMap           as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import           Control.Applicative                  (Alternative)
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch, MonadThrow, throw)
import           Control.Monad                        (when)
import           Data.Copointed                       (Copointed (copoint))
import           Data.Either                          (fromRight)
import qualified Data.Functor.Barbie.Layered          as B
import           Data.Functor.F                       (F, unf)
import           Data.Functor.Identity                (Identity)
import           Data.Functor.With                    (With)
import qualified Data.Functor.With                    as W
import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as N
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe, mapMaybe)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Typeable                        as Y
import           GHC.Generics                         (Generic)
import           GHC.Stack                            (HasCallStack)
import qualified System.FilePath                      as F
import           System.FilePath                      (isPathSeparator, pathSeparator)

compile
  :: ( Alternative m
     , MonadCatch m
     , B.FunctorB et
     , B.FunctorB ev
     , Ord (et (With KSC.Location))
     , Ord (ev (With KSC.Location))
     , KSC.Pretty (et Identity)
     , KSC.Pretty (ev Identity)
     , HasCallStack
     )
  => [KP.EmbeddedParser (With KSC.Location)]
  -> (F (With KSC.Location) (KSC.EmbeddedType (With KSC.Location)) -> Maybe (F (With KSC.Location) (et (With KSC.Location)))) -- ^ Embedded type filter.
  -> (F (With KSC.Location) (KSC.EmbeddedValue (With KSC.Location)) -> Maybe (F (With KSC.Location) (ev (With KSC.Location)))) -- ^ Embedded value filter.
  -> ([F (With KSC.Location) (KS5.Module et ev (With KSC.Location))] -> Map KSC.ModuleName FilePath -> m ()) -- ^ Last step.
  -> (FilePath -> m FilePath) -- ^ File finder.
  -> (FilePath -> m Text) -- ^ File reader.
  -> (Text -> m ()) -- ^ Logger.
  -> FilePath -- ^ Source file path.
  -> m ()
compile _ _ _ _ _ _ _ src@('.' : '.' : _) = throw $ DotDotPathException src
compile embeddedParsers isEmbeddedType isEmbeddedValue lastStep findFile readFile writeLog src = do
  (nameDeps, modules1, paths) <- readRecursively embeddedParsers isEmbeddedType isEmbeddedValue findFile readFile src
  mapM_ (writeLog . ("original module: " <>) . \(n, s) -> KSC.pretty n <> ": " <> KSC.pretty (KSC.toIdentity $ unf s)) $ M.toList modules1
  let (boundValueIdentifiers, boundTypeIdentifiers) = KBN.boundIdentifiers modules1
  modules2 <- mapM (KBN.nameResolve boundValueIdentifiers boundTypeIdentifiers) modules1
  mapM_ (writeLog . ("name resolved: " <>) . \(n, s) -> KSC.pretty n <> ": " <> KSC.pretty (KSC.toIdentity $ unf s)) $ M.toList modules2
  let deps = G.gmap (fromMaybe (X.unreachable "name deps") . flip M.lookup modules2) nameDeps
  sortedModules <- sortModules deps
  modules3 <- snd <$> foldr (accumulate $ flip typeCheck) (pure mempty) sortedModules
  mapM_ (writeLog . ("typed module: " <>) . KSC.pretty . KSC.toIdentity . unf) modules3
  modules4 <- mapM (build1 writeLog) modules3
  lastStep modules4 paths
  where
    accumulate f v acc = do
      (acc1, acc2) <- acc
      (v1, v2) <- f v acc1
      pure (M.union acc1 v1, v2 : acc2)

readRecursively
  :: (Alternative m, MonadCatch m, HasCallStack)
  => [KP.EmbeddedParser (With KSC.Location)]
  -> (F (With KSC.Location) (KSC.EmbeddedType (With KSC.Location)) -> Maybe (F (With KSC.Location) (et (With KSC.Location))))
  -> (F (With KSC.Location) (KSC.EmbeddedValue (With KSC.Location)) -> Maybe (F (With KSC.Location) (ev (With KSC.Location))))
  -> (FilePath -> m FilePath)
  -> (FilePath -> m Text)
  -> FilePath
  -> m
       ( G.AdjacencyMap KSC.ModuleName
       , Map KSC.ModuleName (F (With KSC.Location) (KS1.Module et ev (With KSC.Location)))
       , Map KSC.ModuleName FilePath
       )
readRecursively embeddedParsers isEmbeddedType isEmbeddedValue findFile readFile =
  go (pure (G.empty, M.empty, M.empty))
  where
    go acc path = do
      module' <- do
        let kmPath = F.addExtension path "s.km"
        realPath <- findFile kmPath
        srcText <- readFile realPath
        KP.parse embeddedParsers isEmbeddedType isEmbeddedValue realPath srcText
      let
        KS1.Module moduleName deps _ = copoint module'
        moduleName' = copoint moduleName
        moduleName'' = filePathToModuleName path
        deps' = copoint <$> copoint deps
      when (moduleName' /= "main" && moduleName' /= moduleName'') $ throw $ ModuleNameMismatchException path moduleName' $ W.mayGet moduleName
      (g, m, f) <- acc
      let
        m' = M.insert moduleName' module' m -- TODO 今 main モジュールで、すでに main モジュールがあったらエラー
        f' = M.insert moduleName' (normalisePath path) f
        g' = g `G.overlay` (G.vertex moduleName' `G.connect` G.overlays (G.vertex <$> deps'))
        depSet = S.map (moduleNameToFilePath . copoint) $ S.fromList $ copoint deps
        readSet = S.map moduleNameToFilePath $ M.keysSet m'
      foldl go (pure (g', m', f')) $ depSet S.\\ readSet

sortModules
  :: ( MonadThrow m
     , Ord (et (With KSC.Location))
     , Ord (ev (With KSC.Location))
     , HasCallStack
     )
  => G.AdjacencyMap (F (With KSC.Location) (KS2.Module et ev (With KSC.Location)))
  -> m [F (With KSC.Location) (KS2.Module et ev (With KSC.Location))]
sortModules deps =
  mapM (go . GN.vertexList1) $ fromRight (KE.unreachable "empty") (G.topSort $ G.scc deps)
  where
    go ms@(m :| ms') = if null ms' then pure m else throw $ RecursionException $ (\(KS2.Module n _ _) -> copoint n) . copoint <$> ms

typeCheck
  :: ( MonadCatch m
     , B.FunctorB et
     , B.FunctorB ev
     )
  => Map KSC.QualifiedIdentifier (F (With KSC.Location) (KST1.Type (With KSC.Location)))
  -> F (With KSC.Location) (KS2.Module et ev (With KSC.Location))
  -> m
       ( Map KSC.QualifiedIdentifier (F (With KSC.Location) (KST1.Type (With KSC.Location)))
       , F (With KSC.Location) (KS3.Module et ev (With KSC.Location))
       )
typeCheck types module' = do
  module'' <- KBT.typeCheck types module'
  let
    KS3.Module _ _ ms = copoint module''
    types' =
      M.fromList $ mapMaybe go $ copoint ms
      where
        go m =
          case copoint m of
            KS3.ValueBind i v          -> let KS3.TypedValue _ t = copoint v in Just (i, t)
            KS3.ForeignValueBind i _ t -> Just (i, t)
            _                          -> Nothing
  pure (M.mapKeys copoint types', module'')

build1
  :: ( Applicative m
     , B.FunctorB et
     , B.FunctorB ev
     , KSC.Pretty (et Identity)
     , KSC.Pretty (ev Identity)
     )
  => (Text -> m ())
  -> F (With KSC.Location) (KS3.Module et ev (With KSC.Location))
  -> m (F (With KSC.Location) (KS5.Module et ev (With KSC.Location)))
build1 writeLog m2 = do
  let m3 = KBU.uncurry m2
  writeLog $ "uncurried module: " <> KSC.pretty (KSC.toIdentity $ unf m3)
  let m4 = KBP.partiallyApply m3
  writeLog $ "non-partial-application module: " <> KSC.pretty (KSC.toIdentity $ unf  m4)
  let m5 = KBL.lambdaLift m4
  writeLog $ "lambda-lifted module: " <> KSC.pretty (KSC.toIdentity $ unf m5)
  pure m5

filePathToModuleName :: HasCallStack => FilePath -> KSC.ModuleName
filePathToModuleName path =
  KSC.ModuleName $ fromMaybe (X.unreachable "empty") $ N.nonEmpty $ removeDotDirectory $ T.split isPathSeparator $ T.pack path
  where
    removeDotDirectory []         = []
    removeDotDirectory ("." : xs) = removeDotDirectory xs
    removeDotDirectory (x : xs)   = x : removeDotDirectory xs

moduleNameToFilePath :: KSC.ModuleName -> FilePath
moduleNameToFilePath (KSC.ModuleName n) = T.unpack $ T.intercalate (T.singleton pathSeparator) $ N.toList n

normalisePath :: FilePath -> FilePath
normalisePath =
  fmap go . F.normalise
  where
    go '\\' = '/'
    go c    = c

data Exception
  = RecursionException (N.NonEmpty KSC.ModuleName)
  | ModuleNameMismatchException FilePath KSC.ModuleName (Maybe KSC.Location)
  | DotDotPathException FilePath
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . KE.Exception
  fromException e = do
    KE.Exception e <- E.fromException e
    Y.cast e
