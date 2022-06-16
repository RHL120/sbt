module Script where

import Control.Monad (fail)
import qualified Tester
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

stringLiteralP :: Parser String
stringLiteralP = undefined

argumentsP :: Parser [String]
argumentsP =
  Parsec.char '(' *> Parsec.spaces *>
  Parsec.sepBy stringLiteralP (Parsec.char ',') <*
  Parsec.spaces <*
  Parsec.char ')'

condtionP :: Parser Tester.Condition
condtionP = do
  cc <- Tester.condCons
  args <- argumentsP
  return (const True)

testBodyP :: Parser [Tester.Condition]
testBodyP =
  Parsec.char '{' *> Parsec.spaces *> Parsec.sepBy condtionP Parsec.newline <*
  Parsec.spaces <*
  Parsec.char '}'

testP :: Parser Tester.Test
testP = do
  args <- argumentsP
  if length args /= 2
    then fail "Usage: (name, cmd)"
    else do
      return $ Tester.Test (head args) (last args) []

parseScript :: String -> Tester.Script
parseScript = error "Todo: implement parseScript"
