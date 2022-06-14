module Tester where

import Data.List (isInfixOf)
import qualified Data.Map as Map
import Text.Printf (printf)

type Command = String

type Condition = String -> Bool

type ConditionConstructor
   = [String] -> Either String (String -> Either Error String)

type Error = String

data Test =
  Test
    { testName :: String
    , testConditions :: [ConditionConstructor]
    , testCommand :: String
    }

type Script = [Test]

contains :: ConditionConstructor
contains [] = Left "Expected one argument"
contains (x:xs) =
  Right $ \y ->
    if x `isInfixOf` y
      then Right $ printf "%s does contain %s" y x
      else Left $ printf "%s does not contain %s" x y

conditionsConstructors :: Map.Map String ConditionConstructor
conditionsConstructors = Map.fromList [("contains", contains)]

runTest :: Test -> IO (Either Error String)
runTest = error "TODO: implement runTest"

runScript :: Script -> IO [Either Error String]
runScript = error "TODO: implement runScript"
