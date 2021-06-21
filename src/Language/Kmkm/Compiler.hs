{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Kmkm.Compiler
  ( compile
  , buildC
  ) where

import qualified Language.Kmkm.Builder.C.Pass1 as BC1
import qualified Language.Kmkm.Builder.C.Pass2 as BC2
import qualified Language.Kmkm.Builder.C.Pass3 as BC3
import qualified Language.Kmkm.Builder.C.Pass4 as BC4
import qualified Language.Kmkm.Builder.C.Pass5 as BC5
import qualified Language.Kmkm.Builder.Pass1   as B1
import qualified Language.Kmkm.Builder.Pass2   as B2
import qualified Language.Kmkm.Builder.Pass3   as B3
import qualified Language.Kmkm.Builder.Pass4   as B4
import           Language.Kmkm.Config          (Config (Config, headers))
import           Language.Kmkm.Exception       (unreachable)
import qualified Language.Kmkm.Exception       as X
import           Language.Kmkm.Parser.Sexp     (parse)
import           Language.Kmkm.Syntax          (ModuleName (ModuleName), QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax          as S
import qualified Language.Kmkm.Syntax.Phase1   as P1
import qualified Language.Kmkm.Syntax.Phase2   as P2

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
import           Data.Maybe                           (mapMaybe)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Typeable                        as Y
import           GHC.Generics                         (Generic)
import qualified Language.C.Pretty                    as C
import           Language.C.Syntax.AST                (CTranslUnit)
import           System.FilePath                      (isPathSeparator, pathSeparator)
import qualified System.FilePath                      as F
import qualified Text.PrettyPrint                     as P

compile :: MonadCatch m => Config -> (FilePath -> m Text) -> (FilePath -> Text -> m ()) -> (Text -> m ()) -> FilePath -> m ()
compile _ _ _ _ src@('.' : '.' : _) = throwM $ DotDotPathException src
compile config readFile writeFile writeLog src = do
  let src' = F.normalise src
  (deps, modules1) <- readRecursively readFile src'
  orderedModuleNames <- sequence $ (\ms@(m :| ms') -> if null ms' then pure m else throwM $ RecursionException ms) . GN.vertexList1 <$> fromRight unreachable (G.topSort $ G.scc deps)
  (_, modules2) <- foldr (eachModule modules1) (pure (M.empty, M.empty)) orderedModuleNames
  docs <- sequence $ build config writeLog <$> modules2
  sequence_ $ M.mapWithKey write docs
  where
    eachModule modules1 moduleName acc = do
      (types, modules2) <- acc
      m@(S.Module _ _ ms) <- B1.typeCheck types $ modules1 M.! moduleName -- (M.!) never fail
      let
        types' =
          M.fromList $ mapMaybe go ms
          where
            go (S.ValueBind (S.BindU i (S.TypedTerm _ t))) = Just (QualifiedIdentifier (Just moduleName) i, t)
            go _                                           = Nothing
      pure (types `M.union` types', M.insert moduleName m modules2)
    write k (c, h) = do
      let path = moduleNameToFilePath k
      writeFile (F.addExtension path "c") $ T.pack $ P.render c
      writeFile (F.addExtension path "h") $ T.pack $ P.render h

readRecursively :: MonadThrow m => (FilePath -> m Text) -> FilePath -> m (G.AdjacencyMap ModuleName, Map ModuleName P1.Module)
readRecursively readFile =
  go $ pure (G.empty, M.empty)
  where
    go acc path = do
      module'@(S.Module moduleName deps _) <- parse path =<< readFile (F.addExtension path "s.km")
      let moduleName' = filePathToModuleName path
      when (moduleName /= moduleName') $ throwM $ ModuleNameMismatchException path moduleName
      (g, m) <- acc
      let
        m' = M.insert moduleName module' m
        g' = g `G.overlay` (G.vertex moduleName `G.connect` G.overlays (G.vertex <$> deps))
        depSet = S.map moduleNameToFilePath $ S.fromList deps
        readSet = S.map moduleNameToFilePath $ M.keysSet m'
      foldl go (pure (g', m')) $ depSet S.\\ readSet

filePathToModuleName :: FilePath -> ModuleName
filePathToModuleName path = ModuleName $ N.fromList $ T.split isPathSeparator $ T.pack path -- N.fromList never fail

moduleNameToFilePath :: ModuleName -> FilePath
moduleNameToFilePath (ModuleName n) = T.unpack $ T.intercalate (T.singleton pathSeparator) $ N.toList n

moduleNameToHeaderPath :: ModuleName -> S.CHeader
moduleNameToHeaderPath (ModuleName n) = S.LocalHeader $ T.intercalate "/" (N.toList n) <> ".h"

build :: Monad m => Config -> (Text -> m ()) -> P2.Module -> m (P.Doc, P.Doc)
build config@Config { headers } writeLog m@(S.Module n@(ModuleName i) ms _) = do
  (hs, c, h) <- buildC config writeLog m
  let
    key = P.text $ T.unpack $ T.toUpper (T.intercalate "_" $ N.toList i) <> "_H"
    newline = P.char '\n'
    include (S.SystemHeader h) = P.text $ T.unpack $ "#include <" <> h <> ">\n"
    include (S.LocalHeader h)  = P.text $ T.unpack $ "#include \"" <> h <> "\"\n"
  pure
    ( mconcat $
        (include . moduleNameToHeaderPath <$> n : ms) ++
        (include <$> headers ++ hs) ++
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
        (include <$> headers ++ hs) ++
        (include . moduleNameToHeaderPath <$> ms) ++
        [ C.pretty h
        , newline
        , P.text "#endif"
        , newline
        ]
    )

buildC :: Applicative m => Config -> (Text -> m ()) -> P2.Module -> m ([S.CHeader], CTranslUnit, CTranslUnit)
buildC config writeLog m2 = do
  writeLog $ "typed module: " <> T.pack (show m2)
  let m3 = B2.uncurry m2
  writeLog $ "uncurried module: " <> T.pack (show m3)
  let m4 = B3.partialApplication m3
  writeLog $ "non-partial-application module: " <> T.pack (show m4)
  let m5 = B4.lambdaLifting m4
  writeLog $ "lambda-lifted module: " <> T.pack (show m5)
  let m6 = BC1.thunk m5
  writeLog $ "thunk module: " <> T.pack (show m6)
  let m7 = BC2.translate config m6
  writeLog $ "abstract C file: " <> T.pack (show $ snd m7)
  let (hs, c) = second BC3.simplify m7
  writeLog $ "simplified abstract C file: " <> T.pack (show c)
  pure (hs, BC5.translate c, BC5.translate $ BC4.header c)

data Exception
  = RecursionException (N.NonEmpty ModuleName)
  | ModuleNameMismatchException FilePath ModuleName
  | DotDotPathException FilePath
  deriving (Show, Read, Eq, Ord, Generic)

instance E.Exception Exception where
  toException = E.toException . X.Exception
  fromException e = do
    X.Exception e <- E.fromException e
    Y.cast e

