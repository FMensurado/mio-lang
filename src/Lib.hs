module Lib where

import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.List (intercalate)
import System.Console.Haskeline
import System.Environment
import System.IO

import Parser (parseToplevel)

runCompiler :: IO ()
runCompiler = putStrLn "Hello"

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
