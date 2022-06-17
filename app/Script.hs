module Script
  ( parseScript
  ) where

import Control.Applicative
import Control.Monad (fail)
import qualified Tester
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

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

condtionP :: Parser Tester.Condition
condtionP = do
  cc <- Tester.condCons
  Parsec.spaces
  args <- argumentsP
  case cc args of
    Left e -> fail ("error: " ++ e)
    Right f -> return f

testBodyP :: Parser [Tester.Condition]
testBodyP =
  Parsec.between
    (Parsec.char '{' <* Parsec.spaces)
    (Parsec.char '}')
    (many (condtionP <* Parsec.spaces))

testP :: Parser Tester.Test
testP = do
  args <- argumentsP
  Parsec.spaces
  Parsec.string "should"
  Parsec.spaces
  if length args /= 2
    then fail "Usage: (name, cmd)"
    else Tester.Test (head args) (last args) <$> testBodyP

parseScript :: String -> Either Parsec.ParseError Tester.Script
parseScript =
  Parsec.parse
    (some (Parsec.spaces *> testP <* Parsec.spaces))
    "Failed to parse script"
