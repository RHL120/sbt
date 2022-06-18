{-# LANGUAGE LambdaCase #-}

module Script
  ( parseScript
  , runScript
  , Condition
  , condtionP
  ) where

import Control.Applicative
import Control.Monad (fail)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.Internals (withAllHandles__)
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

type Script = [Test]

instance Show TestResult where
  show (TestResult (Test n _ _) output True) = printf "Test '%s' passed" n
  show (TestResult (Test n _ _) output False) =
    printf "Test '%s' failed with output '%s'" n output

-- There is a way to simplify this. In map we can add a wrapper to the constructor
-- which checks the length of the arguments for us but on the other hand we will
-- have to use !! alot or we can use do something like this \[x] -> but then
-- we will have to ignore possibel warnings saying that \[] -> is not defined
condCons :: Parser ConditionConstructor
condCons =
  Parsec.choice $
  map
    (\(f, n) -> f <$ Parsec.try (Parsec.string n))
    [ ( \case
          [x] -> Right (isInfixOf x)
          _ -> Left $ printf "Usage: contains(<value to be contained>)"
      , "contain")
    , ( \case
          [] -> Right (== "")
          _ -> Left "Usage: be_empty()"
      , "be_empty")
    , ( \case
          [x] ->
            if not $ all isDigit x
              then Left "expected length must be an int"
              else Right (\l -> length l == length x)
          _ -> Left "Usage: have_len(<expected length>)"
      , "have_len")
    , ( \case
          [arg] -> do
            h <-
              either
                (Left . show)
                Right
                (Parsec.parse condtionP "failed to parse condition" arg)
            return (not . h)
          _ -> Left "Usage: not(condition)"
      , "not")
    , ( \case
          [arg] -> Right (isSuffixOf arg)
          _ -> Left "Usage: have_suffix(<suffix>)"
      , "have_suffix")
    , ( \case
          [arg] -> Right (isPrefixOf arg)
          _ -> Left "Usage: have_prefix(<prefix>)"
      , "have_prefix")
    , ( \case
          [arg] -> Right (arg ==)
          _ -> Left "Usage: be(<str to compare to>)"
      , "be")
    ]

runTest :: Test -> IO TestResult
runTest t@(Test _ cmd conds) = do
  (_, Just out, _, handle) <- createProcess (shell cmd) {std_out = CreatePipe}
  waitForProcess handle
  output <- hGetContents out
  return (TestResult t output (all (\f -> f output) conds))

runScript :: Script -> IO [TestResult]
runScript = traverse runTest

stringLiteralP :: Parser String
stringLiteralP =
  Parsec.char '"' *> many (Parsec.try ('"' <$ Parsec.string "\\\"") <|> ec) <*
  Parsec.char '"'
  where
    ec = Parsec.noneOf "\"" :: Parser Char

argumentsP :: Parser [String]
argumentsP =
  Parsec.char '(' *> Parsec.spaces *>
  Parsec.sepBy
    stringLiteralP
    (Parsec.spaces *> Parsec.char ',' <* Parsec.spaces) <*
  Parsec.spaces <*
  Parsec.char ')'

condtionP :: Parser Condition
condtionP = do
  cc <- condCons
  Parsec.spaces
  args <- argumentsP
  case cc args of
    Left e -> fail ("error: " ++ e)
    Right f -> return f

testBodyP :: Parser [Condition]
testBodyP =
  Parsec.between
    (Parsec.char '{' <* Parsec.spaces)
    (Parsec.char '}')
    (many (condtionP <* Parsec.spaces))

testP :: Parser Test
testP = do
  args <- argumentsP
  Parsec.spaces
  Parsec.string "should"
  Parsec.spaces
  if length args /= 2
    then fail "Usage: (name, cmd)"
    else Test (head args) (last args) <$> testBodyP

parseScript :: String -> Either Parsec.ParseError Script
parseScript =
  Parsec.parse
    (some (Parsec.spaces *> testP <* Parsec.spaces))
    "Failed to parse script"
