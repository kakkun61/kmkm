{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
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
import qualified Language.Kmkm.Parse.Sexp            as KP
import qualified Language.Kmkm.Syntax                as KS
import qualified Language.Kmkm.Exception as X

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
import           Data.Maybe                           (mapMaybe, fromMaybe)
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
  let (boundValueIdentifiers, boundTypeIdentifiers) = KBN.boundIdentifiers modules1
  modules2 <- sequence $ KBN.nameResolve boundValueIdentifiers boundTypeIdentifiers <$> modules1
  let deps = G.gmap (fromMaybe X.unreachable . flip M.lookup modules2) nameDeps
  sortedModules <- sortModules deps
  modules3 <- snd <$> foldr (accumulate typeCheck) (pure mempty) sortedModules
  modules4 <- sequence $ build1 writeLog <$> S.toList modules3
  let typeOrigins = M.unions $ KBCI.typeOrigins <$> modules4
  docs <- sequence $ (\m@(KS.Module n _ _) -> do { ds <- build2 writeLog typeOrigins m; pure (n, ds) }) <$> modules4
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

readRecursively :: MonadThrow m => (FilePath -> m Text) -> FilePath -> m (G.AdjacencyMap KS.ModuleName, Map KS.ModuleName (KS.Module 'KS.NameUnresolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped))
readRecursively readFile =
  go $ pure (G.empty, M.empty)
  where
    go acc path = do
      module'@(KS.Module moduleName deps _) <- KP.parse path =<< readFile (F.addExtension path "s.km")
      let moduleName' = filePathToModuleName path
      when (moduleName /= moduleName') $ throwM $ ModuleNameMismatchException path moduleName
      (g, m) <- acc
      let
        m' = M.insert moduleName module' m
        g' = g `G.overlay` (G.vertex moduleName `G.connect` G.overlays (G.vertex <$> deps))
        depSet = KS.map moduleNameToFilePath $ KS.fromList deps
        readSet = KS.map moduleNameToFilePath $ M.keysSet m'
      foldl go (pure (g', m')) $ depSet KS.\\ readSet

sortModules :: MonadThrow m => G.AdjacencyMap (KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped) -> m [KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped]
sortModules deps =
  sequence $ (\ms@(m :| ms') -> if null ms' then pure m else throwM $ RecursionException $ (\(KS.Module n _ _) -> n) <$> ms) . GN.vertexList1 <$> fromRight KE.unreachable (G.topSort $ G.scc deps)

typeCheck
  :: MonadCatch m
  => KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Untyped
  -> Map KS.QualifiedIdentifier (KS.Type 'KS.NameResolved 'KS.Curried)
  -> m (Map KS.QualifiedIdentifier (KS.Type 'KS.NameResolved 'KS.Curried), KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Typed)
typeCheck module' types = do
  module''@(KS.Module _ _ ms) <- KBT.typeCheck types module'
  let
    types' =
      M.fromList $ mapMaybe go ms
      where
        go (KS.ValueBind (KS.ValueBindU i (KS.TypedTerm _ t))) = Just (i, t)
        go _                                                   = Nothing
  pure (types', module'')

build2 :: Monad m => (Text -> m ()) -> Map KS.QualifiedIdentifier KBCI.TypeOrigin -> KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed -> m (P.Doc, P.Doc)
build2 writeLog typeOrigins m@(KS.Module n@(KS.ModuleName i) ms _) = do
  (hs, c, h) <- build2' writeLog typeOrigins m
  let
    key = P.text $ T.unpack $ T.toUpper (T.intercalate "_" $ N.toList i) <> "_H"
    newline = P.char '\n'
    include (KS.SystemHeader h) = P.text $ T.unpack $ "#include <" <> h <> ">\n"
    include (KS.LocalHeader h)  = P.text $ T.unpack $ "#include \"" <> h <> "\"\n"
  pure
    ( mconcat $
        (include . moduleNameToHeaderPath <$> n : ms) ++
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
        (include . moduleNameToHeaderPath <$> ms) ++
        [ C.pretty h
        , newline
        , P.text "#endif"
        , newline
        ]
    )

build1
  :: Applicative m
  => (Text -> m ())
  -> KS.Module 'KS.NameResolved 'KS.Curried 'KS.LambdaUnlifted 'KS.Typed
  -> m (KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed)
build1 writeLog m2 = do
  writeLog $ "typed module: " <> T.pack (show m2)
  let m3 = KBU.uncurry m2
  writeLog $ "uncurried module: " <> T.pack (show m3)
  let m4 = KBP.partiallyApply m3
  writeLog $ "non-partial-application module: " <> T.pack (show m4)
  let m5 = KBL.lambdaLift m4
  writeLog $ "lambda-lifted module: " <> T.pack (show m5)
  let m6 = KBCT.thunk m5
  writeLog $ "thunk module: " <> T.pack (show m6)
  pure m6

build2'
  :: Applicative m
  => (Text -> m ())
  -> Map KS.QualifiedIdentifier KBCI.TypeOrigin
  -> KS.Module 'KS.NameResolved 'KS.Uncurried 'KS.LambdaLifted 'KS.Typed
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
  | ModuleNameMismatchException FilePath KS.ModuleName
  | DotDotPathException FilePath
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . KE.Exception
  fromException e = do
    KE.Exception e <- E.fromException e
    Y.cast e

