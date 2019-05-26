module Lib where

import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.List (intercalate)
import System.Console.Haskeline
import System.Environment
import System.IO

import Parser (parseToplevel)

compile :: String -> String
compile x =
  case parseToplevel x of
    Right y -> init $ intercalate "" $ map ((++ "\n\n") . show) y
    Left e -> show e

type FileBase = String

type FileExt = String

type FilePathInfo = (FileBase, FileExt)

parseName :: String -> FilePathInfo
parseName x =
  let x' = reverse x
      f = (/= '.')
      l = init $ reverse $ dropWhile f x'
      r = reverse $ takeWhile f x'
   in (l, r)

compileIO :: String -> ExceptT String IO (IO ())
compileIO file = do
  let (base, ext) = parseName file
  if ext /= "mio"
    then throwError "Input file must has an extension of \".mio\"."
    else do
      x <- liftIO $ readFile file
      return $ writeFile (base ++ ".mips") $ compile x

resultReporter :: Either String (IO ()) -> IO ()
resultReporter (Left e) = putStrLn $ "Compile failed with exception:\n" ++ e
resultReporter (Right a) = putStrLn "Compile Success." >>= const a

runCompiler :: IO ()
runCompiler = do
  let run = runExceptT . compileIO . head
  y <- getArgs
  x <- run y
  resultReporter x

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
