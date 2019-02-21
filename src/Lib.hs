module Lib
  ( runCompiler
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Except
import System.Environment
import System.IO

import Generator (generator)
import Parser (simpleParser)

sourceCode :: String
sourceCode = "14 + 2 - 5"

compile :: String -> String
compile = generator . simpleParser

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
