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
import qualified Language.Kmkm.Builder.Pass1   as B1
import qualified Language.Kmkm.Builder.Pass2   as B2
import qualified Language.Kmkm.Builder.Pass3   as B3
import qualified Language.Kmkm.Builder.Pass4   as B4
import           Language.Kmkm.Config          (Config (Config, headers))
import           Language.Kmkm.Exception       (unreachable)
import qualified Language.Kmkm.Exception       as X
import           Language.Kmkm.Parser.Sexp     (parse)
import qualified Language.Kmkm.Syntax          as S
import           Language.Kmkm.Syntax.Base     (ModuleName (ModuleName), QualifiedIdentifier (QualifiedIdentifier))
import qualified Language.Kmkm.Syntax.Phase1   as P1
import qualified Language.Kmkm.Syntax.Phase2   as P2
import qualified Language.Kmkm.Syntax.Value    as V

import qualified Algebra.Graph.AdjacencyMap           as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as GN
import qualified Control.Exception                    as E
import           Control.Monad                        (when)
import           Control.Monad.Catch                  (MonadThrow (throwM))
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

compile :: Config -> (FilePath -> IO Text) -> (FilePath -> Text -> IO ()) -> FilePath -> IO ()
compile _ _ _ src@('.' : '.' : _) = throwM $ DotDotPathException src
compile config readFile writeFile src = do
  let src' = F.normalise src
  (deps, modules1) <- readRecursively readFile src'
  orderedModuleNames <- sequence $ (\ms@(m :| ms') -> if null ms' then pure m else throwM $ RecursionException ms) . GN.vertexList1 <$> fromRight unreachable (G.topSort $ G.scc deps)
  (_, modules2) <- foldr (eachModule modules1) (pure (M.empty, M.empty)) orderedModuleNames
  let docs = build config <$> modules2
  sequence_ $ M.mapWithKey write docs
  where
    eachModule modules1 moduleName acc = do
      (types, modules2) <- acc
      m@(S.Module _ _ ms) <- B1.typeCheck types $ modules1 M.! moduleName -- (M.!) never fail
      let
        types' =
          M.fromList $ mapMaybe go ms
          where
            go (S.ValueBind (S.ValueBindU i (V.TypedTerm _ t)) _) = Just (QualifiedIdentifier (Just moduleName) i, t)
            go _                                                  = Nothing
      pure (types `M.union` types', M.insert moduleName m modules2)
    write k (c, h) = do
      let path = moduleNameToFilePath k
      writeFile (F.addExtension path "c") $ T.pack $ P.render c
      writeFile (F.addExtension path "h") $ T.pack $ P.render h

readRecursively :: (FilePath -> IO Text) -> FilePath -> IO (G.AdjacencyMap ModuleName, Map ModuleName P1.Module)
readRecursively readFile =
  go $ pure (G.empty, M.empty)
  where
    go :: IO (G.AdjacencyMap ModuleName, Map ModuleName P1.Module) -> FilePath -> IO (G.AdjacencyMap ModuleName, Map ModuleName P1.Module)
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

build :: Config -> P2.Module -> (P.Doc, P.Doc)
build config@Config { headers } m@(S.Module (ModuleName i) ms _) =
  let
    (hs, c, h) = buildC config m
    key = P.text $ T.unpack $ T.toUpper (T.intercalate "_" $ N.toList i) <> "_H"
    newline = P.char '\n'
    include (S.SystemHeader h) = P.text $ T.unpack $ "#include <" <> h <> ">\n"
    include (S.LocalHeader h)  = P.text $ T.unpack $ "#include \"" <> h <> "\"\n"
  in
    ( mconcat $
        (include . moduleNameToHeaderPath <$> ms) ++
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

buildC :: Config -> P2.Module -> ([S.CHeader], CTranslUnit, CTranslUnit)
buildC config m =
  let (hs, c) = BC2.convert config $ BC1.convert $ B4.lambdaLifting $ B3.partialApplication $ B2.uncurry m
  in (hs, BC4.convert c, BC4.convert $ BC3.header c)

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

