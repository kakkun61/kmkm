{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Kmkm.Internal.Compile
  ( compile
  , Exception (..)
  ) where

import qualified Language.Kmkm.Internal.Build.LambdaLift     as KBL
import qualified Language.Kmkm.Internal.Build.NameResolve    as KBN
import qualified Language.Kmkm.Internal.Build.PartiallyApply as KBP
import qualified Language.Kmkm.Internal.Build.TypeCheck      as KBT
import qualified Language.Kmkm.Internal.Build.Uncurry        as KBU
import qualified Language.Kmkm.Internal.Exception            as KE
import qualified Language.Kmkm.Internal.Exception            as X
import qualified Language.Kmkm.Internal.Parse.Sexp           as KP
import qualified Language.Kmkm.Internal.Syntax               as KS

import qualified Algebra.Graph.AdjacencyMap           as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Barbies.Bare                         as B (Bare, Covered)
import qualified Barbies.Bare.Layered                 as B (BareB)
import           Control.Applicative                  (Alternative)
import qualified Control.Exception                    as E
import           Control.Exception.Safe               (MonadCatch, MonadThrow, throw)
import           Control.Monad                        (when)
import           Data.Bifunctor                       (Bifunctor (second))
import           Data.Copointed                       (Copointed (copoint))
import           Data.Either                          (fromRight)
import           Data.Functor.Identity                (Identity)
import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as N
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Typeable                        as Y
import           GHC.Generics                         (Generic)
import           System.FilePath                      (isPathSeparator, pathSeparator)
import qualified System.FilePath                      as F

compile
  :: ( Alternative m
     , MonadCatch m
     , Show (et B.Bare Identity)
     , Show (ev B.Bare Identity)
     , B.BareB et
     , B.BareB ev
     , Ord (et B.Covered KS.WithLocation)
     , Ord (ev B.Covered KS.WithLocation)
     )
  => [KP.EmbeddedParser KS.WithLocation]
  -> (KS.WithLocation (KS.EmbeddedType B.Covered KS.WithLocation) -> Maybe (KS.WithLocation (et B.Covered KS.WithLocation))) -- ^ Embedded type filter.
  -> (KS.WithLocation (KS.EmbeddedValue B.Covered KS.WithLocation) -> Maybe (KS.WithLocation (ev B.Covered KS.WithLocation))) -- ^ Embedded value filter.
  -> (Set (KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed et ev B.Covered KS.WithLocation)) -> Map KS.ModuleName FilePath -> m ()) -- ^ Last step.
  -> (FilePath -> m FilePath) -- ^ File finder.
  -> (FilePath -> m Text) -- ^ File reader.
  -> (Text -> m ()) -- ^ Logger.
  -> FilePath -- ^ Source file path.
  -> m ()
compile _ _ _ _ _ _ _ src@('.' : '.' : _) = throw $ DotDotPathException src
compile embeddedParsers isEmbeddedType isEmbeddedValue lastStep findFile readFile writeLog src = do
  (nameDeps, modules1, paths) <- readRecursively embeddedParsers isEmbeddedType isEmbeddedValue findFile readFile src
  sequence_ $ writeLog . ("original module: " <>) . T.pack . show . second KS.strip <$> M.toList modules1
  let (boundValueIdentifiers, boundTypeIdentifiers) = KBN.boundIdentifiers modules1
  modules2 <- sequence $ KBN.nameResolve boundValueIdentifiers boundTypeIdentifiers <$> modules1
  sequence_ $ writeLog . ("name resolved: " <>) . T.pack . show . second KS.strip <$> M.toList modules2
  let deps = G.gmap (fromMaybe X.unreachable . flip M.lookup modules2) nameDeps
  sortedModules <- sortModules deps
  modules3 <- snd <$> foldr (accumulate $ flip typeCheck) (pure mempty) sortedModules
  sequence_ $ writeLog . ("typed module: " <>) . T.pack . show . KS.strip <$> S.toList modules3
  modules4 <- sequence $ build1 writeLog <$> S.toList modules3
  lastStep (S.fromList modules4) paths
  where
    accumulate f v acc = do
      (acc1, acc2) <- acc
      (v1, v2) <- f v acc1
      pure (M.union acc1 v1, S.insert v2 acc2)

readRecursively
  :: (Alternative m, MonadCatch m)
  => [KP.EmbeddedParser KS.WithLocation]
  -> (KS.WithLocation (KS.EmbeddedType B.Covered KS.WithLocation) -> Maybe (KS.WithLocation (et B.Covered KS.WithLocation)))
  -> (KS.WithLocation (KS.EmbeddedValue B.Covered KS.WithLocation) -> Maybe (KS.WithLocation (ev B.Covered KS.WithLocation)))
  -> (FilePath -> m FilePath)
  -> (FilePath -> m Text)
  -> FilePath
  -> m
       ( G.AdjacencyMap KS.ModuleName
       , Map KS.ModuleName (KS.WithLocation (KS.Module 'KS.NameUnresolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped et ev B.Covered KS.WithLocation))
       , Map KS.ModuleName FilePath
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
        KS.Module moduleName deps _ = copoint module'
        moduleName' = copoint moduleName
        moduleName'' = filePathToModuleName path
        deps' = copoint <$> copoint deps
      when (moduleName' /= "main" && moduleName' /= moduleName'') $ throw $ ModuleNameMismatchException path moduleName' $ KS.location moduleName
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
     , Ord (et B.Covered KS.WithLocation)
     , Ord (ev B.Covered KS.WithLocation)
     )
  => G.AdjacencyMap (KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped et ev B.Covered KS.WithLocation))
  -> m [KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped et ev B.Covered KS.WithLocation)]
sortModules deps =
  sequence (go . GN.vertexList1 <$> fromRight KE.unreachable (G.topSort $ G.scc deps))
  where
    go ms@(m :| ms') = if null ms' then pure m else throw $ RecursionException $ (\(KS.Module n _ _) -> copoint n) . copoint <$> ms

typeCheck
  :: ( MonadCatch m
     , B.BareB et
     , B.BareB ev
     )
  => Map KS.QualifiedIdentifier (KS.WithLocation (KS.Type 'KS.NameResolved 'KS.Curried B.Covered KS.WithLocation))
  -> KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped et ev B.Covered KS.WithLocation)
  -> m (Map KS.QualifiedIdentifier (KS.WithLocation (KS.Type 'KS.NameResolved 'KS.Curried B.Covered KS.WithLocation)), KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Typed et ev B.Covered KS.WithLocation))
typeCheck types module' = do
  module'' <- KBT.typeCheck types module'
  let
    KS.Module _ _ ms = copoint module''
    types' =
      M.fromList $ mapMaybe go $ copoint ms
      where
        go m =
          case copoint m of
            KS.ValueBind (KS.ValueBindU i v)
              | KS.TypedValue _ t <- copoint v -> Just (i, t)
            KS.ForeignValueBind i _ t          -> Just (i, t)
            _                                  -> Nothing
  pure (M.mapKeys copoint types', module'')

build1
  :: ( Applicative m
     , Show (et B.Bare Identity)
     , Show (ev B.Bare Identity)
     , B.BareB et
     , B.BareB ev
     )
  => (Text -> m ())
  -> KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Typed et ev B.Covered KS.WithLocation)
  -> m (KS.WithLocation (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed et ev B.Covered KS.WithLocation))
build1 writeLog m2 = do
  let m3 = KBU.uncurry m2
  writeLog $ "uncurried module: " <> T.pack (show $ KS.strip m3)
  let m4 = KBP.partiallyApply m3
  writeLog $ "non-partial-application module: " <> T.pack (show $ KS.strip m4)
  let m5 = KBL.lambdaLift m4
  writeLog $ "lambda-lifted module: " <> T.pack (show $ KS.strip m5)
  pure m5

filePathToModuleName :: FilePath -> KS.ModuleName
filePathToModuleName path = KS.ModuleName $ fromMaybe X.unreachable $ N.nonEmpty $ T.split isPathSeparator $ T.pack path

moduleNameToFilePath :: KS.ModuleName -> FilePath
moduleNameToFilePath (KS.ModuleName n) = T.unpack $ T.intercalate (T.singleton pathSeparator) $ N.toList n

normalisePath :: FilePath -> FilePath
normalisePath =
  fmap go . F.normalise
  where
    go '\\' = '/'
    go c    = c

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
