import qualified Language.Kmkm.Builder.C   as B
import           Language.Kmkm.Parser.Sexp (parse)
import qualified Language.Kmkm.Parser.Sexp as P

import           Control.Exception        (Exception (displayException))
import qualified Control.Exception        as E
import           Control.Exception.Safe   (tryIO)
import           Control.Monad            (unless)
import qualified Data.ByteString.Char8    as B
import           Data.Default.Class       (def)
import           Data.List                (nub, sort)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Traversable         (for)
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
  case parse (T.unpack source) source of
    Left e ->
      case E.fromException e of
        Just (P.Exception m) -> Fail m
        Nothing              -> Fail $ displayException e
    Right m ->
      case B.buildC def m of
        Left e -> Fail $ displayException e
        Right (_, d) ->
          let result = C.pretty d
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
