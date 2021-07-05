{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Compile
  ( compile
  ) where

import qualified Language.Kmkm.Build.C.C             as KBCC
import qualified Language.Kmkm.Build.C.Declare       as KBCD
import qualified Language.Kmkm.Build.C.IntermediateC as KBCI
import qualified Language.Kmkm.Build.C.Simplify      as KBCS
import qualified Language.Kmkm.Build.C.Thunk         as KBCT
import qualified Language.Kmkm.Build.LambdaLift      as KBL
import qualified Language.Kmkm.Build.PartiallyApply  as KBP
import qualified Language.Kmkm.Build.TypeCheck       as KBT
import qualified Language.Kmkm.Build.Uncurry         as KBU
import qualified Language.Kmkm.Exception             as KE
import qualified Language.Kmkm.Exception             as X
import qualified Language.Kmkm.Parse.Sexp            as KP
import qualified Language.Kmkm.Syntax                as KS

import qualified Algebra.Graph.AdjacencyMap           as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Control.Exception                    as E
import           Control.Monad                        (when)
import           Control.Monad.Catch                  (MonadCatch, MonadThrow (throwM))
import           Data.Bifunctor                       (Bifunctor (second))
import           Data.Either                          (fromRight)
import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as N
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe, mapMaybe)
import qualified Data.Set                             as KS
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Typeable                        as Y
import           GHC.Generics                         (Generic)
import qualified Language.C.Pretty                    as C
import           Language.C.Syntax.AST                (CTranslUnit)
import qualified Language.Kmkm.Build.NameResolve      as KBN
import           System.FilePath                      (isPathSeparator, pathSeparator)
import qualified System.FilePath                      as F
import qualified Text.PrettyPrint                     as P

compile :: MonadCatch m => (FilePath -> m Text) -> (FilePath -> Text -> m ()) -> (Text -> m ()) -> FilePath -> m ()
compile _ _ _ src@('.' : '.' : _) = throwM $ DotDotPathException src
compile readFile writeFile writeLog src = do
  let src' = F.normalise src
  (nameDeps, modules1) <- readRecursively readFile src'
  sequence_ $ writeLog . ("original module: " <>) . T.pack . show <$> M.toList modules1
  let (boundValueIdentifiers, boundTypeIdentifiers) = KBN.boundIdentifiers modules1
  modules2 <- sequence $ KBN.nameResolve boundValueIdentifiers boundTypeIdentifiers <$> modules1
  sequence_ $ writeLog . ("name resolved: " <>) . T.pack . show <$> M.toList modules2
  let deps = G.gmap (fromMaybe X.unreachable . flip M.lookup modules2) nameDeps
  sortedModules <- sortModules deps
  modules3 <- snd <$> foldr (accumulate typeCheck) (pure mempty) sortedModules
  sequence_ $ writeLog . ("typed module: " <>) . T.pack . show <$> S.toList modules3
  modules4 <- sequence $ build1 writeLog <$> S.toList modules3
  let typeOrigins = M.unions $ KBCI.typeOrigins <$> modules4
  docs <- sequence $ build2_ typeOrigins <$> modules4
  sequence_ $ write <$> docs
  where
    accumulate f v acc = do
      (acc1, acc2) <- acc
      (v1, v2) <- f v acc1
      pure (M.union acc1 v1, KS.insert v2 acc2)
    write (k, (c, h)) = do
      let
        path = moduleNameToFilePath k
        style = P.Style P.PageMode maxBound 1
      writeFile (F.addExtension path "c") $ T.pack $ P.renderStyle style c
      writeFile (F.addExtension path "h") $ T.pack $ P.renderStyle style h
    build2_ typeOrigins m =
      let KS.Module n _ _ = KS.item m
      in do
        ds <- build2 writeLog typeOrigins m
        pure (KS.item n, ds)

readRecursively
  :: MonadThrow m
  => (FilePath -> m Text)
  -> FilePath
  -> m (G.AdjacencyMap KS.ModuleName, Map KS.ModuleName (KS.AttachPosition (KS.Module 'KS.NameUnresolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped KS.AttachPosition)))
readRecursively readFile =
  go (pure (G.empty, M.empty))
  where
    go acc path = do
      module' <- KP.parse path =<< readFile (F.addExtension path "s.km")
      let
        KS.Module moduleName deps _ = KS.item module'
        moduleName' = KS.item moduleName
        moduleName'' = filePathToModuleName path
        deps' = KS.item <$> deps
      when (moduleName' /= moduleName'') $ throwM $ ModuleNameMismatchException path moduleName' $ KS.range moduleName
      (g, m) <- acc
      let
        m' = M.insert moduleName' module' m
        g' = g `G.overlay` (G.vertex moduleName' `G.connect` G.overlays (G.vertex <$> deps'))
        depSet = KS.map (moduleNameToFilePath . KS.item) $ KS.fromList deps
        readSet = KS.map moduleNameToFilePath $ M.keysSet m'
      foldl go (pure (g', m')) $ depSet KS.\\ readSet

sortModules
  :: MonadThrow m
  => G.AdjacencyMap (KS.AttachPosition (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped KS.AttachPosition))
  -> m [KS.AttachPosition (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped KS.AttachPosition)]
sortModules deps =
  sequence $ go . GN.vertexList1 <$> fromRight KE.unreachable (G.topSort $ G.scc deps)
  where
    go ms@(m :| ms') = if null ms' then pure m else throwM $ RecursionException $ (\(KS.Module n _ _) -> KS.item n) . KS.item <$> ms

typeCheck
  :: MonadCatch m
  => KS.AttachPosition (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped KS.AttachPosition)
  -> Map KS.QualifiedIdentifier (KS.AttachPosition (KS.Type 'KS.NameResolved 'KS.Curried KS.AttachPosition))
  -> m (Map KS.QualifiedIdentifier (KS.AttachPosition (KS.Type 'KS.NameResolved 'KS.Curried KS.AttachPosition)), KS.AttachPosition (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Typed KS.AttachPosition))
typeCheck module' types = do
  module'' <- KBT.typeCheck types module'
  let
    KS.Module _ _ ms = KS.item module''
    types' =
      M.fromList $ mapMaybe go ms
      where
        go m =
          case KS.item m of
            KS.ValueBind (KS.ValueBindU i v)
              | KS.TypedValue _ t <- KS.item v -> Just (i, t)
            _                                                   -> Nothing
  pure (M.mapKeys KS.item types', module'')

build2
  :: ( Monad m
     , KS.HasPosition f
     )
  => (Text -> m ())
  -> Map KS.QualifiedIdentifier KBCI.TypeOrigin
  -> f (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed f)
  -> m (P.Doc, P.Doc)
build2 writeLog typeOrigins m = do
  let KS.Module n ms _ = KS.item m
  (hs, c, h) <- build2' writeLog typeOrigins m
  let
    n'@(KS.ModuleName i) = KS.item n
    ms' = KS.item <$> ms
    key = P.text $ T.unpack $ T.toUpper (T.intercalate "_" $ N.toList i) <> "_H"
    newline = P.char '\n'
    include (KS.SystemHeader h) = P.text $ T.unpack $ "#include <" <> h <> ">\n"
    include (KS.LocalHeader h)  = P.text $ T.unpack $ "#include \"" <> h <> "\"\n"
  pure
    ( mconcat $
        (include . moduleNameToHeaderPath <$> n' : ms') ++
        (include <$> hs) ++
        [ C.pretty c
        , newline
        ]
    , mconcat $
        [ P.text "#ifndef "
        , key
        , newline
        , P.text "#define "
        , key
        , newline
        ] ++
        (include <$> hs) ++
        (include . moduleNameToHeaderPath <$> ms') ++
        [ C.pretty h
        , newline
        , P.text "#endif"
        , newline
        ]
    )

build1
  :: Applicative m
  => (Text -> m ())
  -> KS.AttachPosition (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Typed KS.AttachPosition)
  -> m (KS.AttachPosition (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed KS.AttachPosition))
build1 writeLog m2 = do
  let m3 = KBU.uncurry m2
  writeLog $ "uncurried module: " <> T.pack (show m3)
  let m4 = KBP.partiallyApply m3
  writeLog $ "non-partial-application module: " <> T.pack (show m4)
  let m5 = KBL.lambdaLift <$> m4
  writeLog $ "lambda-lifted module: " <> T.pack (show m5)
  let m6 = KBCT.thunk <$> m5
  writeLog $ "thunk module: " <> T.pack (show m6)
  pure m6

build2'
  :: ( Applicative m
     , KS.HasPosition f)
  => (Text -> m ())
  -> Map KS.QualifiedIdentifier KBCI.TypeOrigin
  -> f (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed f)
  -> m ([KS.CHeader], CTranslUnit, CTranslUnit)
build2' writeLog typeOrigins m6 = do
  let m7 = KBCI.translate typeOrigins m6
  writeLog $ "abstract C file: " <> T.pack (show $ snd m7)
  let (hs, c) = second KBCS.simplify m7
  writeLog $ "simplified abstract C file: " <> T.pack (show c)
  pure (hs, KBCC.translate c, KBCC.translate $ KBCD.declare c)

filePathToModuleName :: FilePath -> KS.ModuleName
filePathToModuleName path = KS.ModuleName $ fromMaybe X.unreachable $ N.nonEmpty $ T.split isPathSeparator $ T.pack path

moduleNameToFilePath :: KS.ModuleName -> FilePath
moduleNameToFilePath (KS.ModuleName n) = T.unpack $ T.intercalate (T.singleton pathSeparator) $ N.toList n

moduleNameToHeaderPath :: KS.ModuleName -> KS.CHeader
moduleNameToHeaderPath (KS.ModuleName n) = KS.LocalHeader $ T.intercalate "/" (N.toList n) <> ".h"

data Exception
  = RecursionException (N.NonEmpty KS.ModuleName)
  | ModuleNameMismatchException FilePath KS.ModuleName (Maybe (KS.Position, KS.Position))
  | DotDotPathException FilePath
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . KE.Exception
  fromException e = do
    KE.Exception e <- E.fromException e
    Y.cast e

