module Main where

import Script (parseScript)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO
import Tester (runScript, runTest)
import Text.Printf (printf)

getScriptPath :: IO String
getScriptPath = do
  args <- getArgs
  pname <- getProgName
  if length args /= 1
    then do
      pname <- getProgName
      putStrLn $ "Usage: " ++ pname ++ " <script path>"
      exitFailure
    else return $ head args

main :: IO ()
main = do
  path <- getScriptPath
  script <- parseScript <$> (openFile path ReadMode >>= hGetContents)
  case script of
    Left e -> putStrLn ("Failed to parse script error " ++ show e)
    Right scr -> do
      res <- runScript scr
      () <$ traverse print res
