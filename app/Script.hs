module Script where

import Control.Applicative
import Control.Monad (fail)
import qualified Tester
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser(stringLiteral))

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
  case cc args of
    Left e -> fail ("error: " ++ e)
    Right f -> return f

testBodyP :: Parser [Tester.Condition]
testBodyP =
  Parsec.char '{' *> Parsec.spaces *>
  Parsec.sepBy condtionP (Parsec.string "and") <*
  Parsec.spaces <*
  Parsec.char '}'

testP :: Parser Tester.Test
testP = do
  args <- argumentsP
  if length args /= 2
    then fail "Usage: (name, cmd)"
    else Tester.Test (head args) (last args) <$> testBodyP

parseScript :: String -> Tester.Script
parseScript = error "Todo: implement parseScript"
