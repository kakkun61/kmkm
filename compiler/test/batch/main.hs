import qualified Language.Kmkm.Build.NameResolve as KN
import qualified Language.Kmkm.Build.TypeCheck   as KT
import qualified Language.Kmkm.Compile           as KC
import qualified Language.Kmkm.Parse.Sexp        as KP
import qualified Language.Kmkm.Syntax            as KS

import           Control.Exception        (Exception (displayException))
import qualified Control.Exception        as E
import           Control.Exception.Safe   (tryIO)
import           Control.Monad            (unless)
import qualified Data.ByteString.Char8    as B
import           Data.Default.Class       (def)
import           Data.Functor.Identity    (Identity (runIdentity))
import           Data.List                (nub, sort)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Traversable         (for)
import qualified GHC.Exts                 as X
import           Language.C               (parseC)
import qualified Language.C.Data.Position as C
import qualified Language.C.Pretty        as C
import           Main.Utf8                (withUtf8)
import           System.Console.ANSI      (Color (Green, Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                                           SGR (Reset, SetColor), setSGR)
import           System.Directory         (listDirectory)
import           System.Exit              (exitFailure)
import qualified Text.PrettyPrint         as P

main :: IO ()
main =
  withUtf8 $ do
    files <- sort . nub . (takeWhile (/= '.') <$>) <$> listDirectory dir
    success <- and <$> for files go
    unless success exitFailure
    where
      dir = "test/batch/asset"
      go file = do
        r <-
          tryIO $ do
            let path = dir <> "/" <> file
            source <- T.readFile $ path <> ".s.km"
            expected <- B.readFile $ path <> ".c"
            case parseC expected $ C.position 0 file 0 0 Nothing of
              Left e -> do
                putStrLnRed $ "ERROR " ++ file
                print e
                pure False
              Right c ->
                case test source $ C.pretty c of
                  Pass -> do
                    putStrLnGreen $ "PASS " ++ file
                    pure True
                  Mismatch r -> do
                    putStrLnRed $ "FAIL " ++ file
                    putStrLn "result:"
                    putStrLn $ P.render r
                    putStrLn "expected:"
                    B.putStrLn expected
                    pure False
                  Fail m -> do
                    putStrLnRed $ "FAIL " ++ file
                    putStrLn m
                    pure False
        case r of
          Right a -> pure a
          Left e -> do
            putStrLnRed $ "ERROR " ++ file
            putStrLn $ displayException e
            pure False

data Result = Pass | Mismatch P.Doc | Fail String

test :: Text -> P.Doc -> Result
test source expected =
  let
    r = do
      m@(KS.Module n _ _) <- KP.parse (T.unpack source) source
      m' <- KN.nameResolve (KN.boundIdentifiers $ X.fromList [(n, m)]) m
      KT.typeCheck mempty m'
  in
    case r of
      Left e ->
        case (E.fromException e, E.fromException e) of
          (Just (KP.Exception m), _)        -> Fail m
          (_, Just (KN.UnknownException i)) -> Fail $ "unknown identifier: " ++ show i
          (Nothing, Nothing)                -> Fail $ displayException e
      Right m ->
        let
          (_, d, _) = runIdentity $ KC.buildC def (const $ pure ()) m
          result = C.pretty d
        in
          if result == expected
            then Pass
            else Mismatch result

putStrLnGreen :: String -> IO ()
putStrLnGreen s = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn s
  setSGR [Reset]

putStrLnRed :: String -> IO ()
putStrLnRed s = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn s
  setSGR [Reset]
