{-# LANGUAGE PatternSynonyms #-}

import qualified Language.Kmkm.Builder.C   as B
import           Language.Kmkm.Parser.Sexp (parse)

import           Control.Monad            (unless)
import           Data.Either.Result       (pattern Error, pattern Success)
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
import Control.Exception (Exception(displayException))
import Control.Exception.Safe (tryIO)

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
            source <- T.readFile $ path <> ".km.lisp"
            expected <- readFile $ path <> ".c"
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
                    putStrLn expected
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
test source expected = do
  case parse (T.unpack source) source of
    Error e -> Fail e
    Success m ->
      let result = C.pretty $ B.buildC m
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
