{-# LANGUAGE LambdaCase #-}

module Tester where

import Control.Applicative
import Data.List (isInfixOf)
import qualified Data.Map as Map
import GHC.IO.Handle (hGetContents)
import System.Process
  ( StdStream(CreatePipe)
  , createProcess
  , shell
  , std_out
  , waitForProcess
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Printf (printf)

type Condition = (String -> Bool)

type ConditionConstructor = [String] -> Either String Condition

data Test =
  Test
    { testName :: String
    , testCmd :: String
    , testConditions :: [Condition]
    }

data TestResult =
  TestResult
    { testedTest :: Test
    , testedOutput :: String
    , result :: Bool
    }
  deriving (Show)

type Script = [Test]

instance Show Test where
  show (Test name cmd _) = printf "name=%s, cmd=%s" name cmd

condCons :: Parser ConditionConstructor
condCons =
  (\case
     [x] -> Right (isInfixOf x)
     _ -> Left $ printf "Usage: contains(<value to be contained>)") <$
  Parsec.string "contain" <|>
  (\case
     [] -> Right (== "")
     _ -> Left "Usage: be_empty()") <$
  Parsec.string "be_empty"

runTest :: Test -> IO TestResult
runTest t@(Test _ cmd conds) = do
  (_, Just out, _, handle) <- createProcess (shell cmd) {std_out = CreatePipe}
  waitForProcess handle
  output <- hGetContents out
  return (TestResult t "" (all (\f -> f output) conds))

runScript :: Script -> IO [TestResult]
runScript = traverse runTest
