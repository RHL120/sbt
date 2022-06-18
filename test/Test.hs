module Main where

import Control.Exception (assert)
import Data.Either
import Script
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Parsec (parse)

getCond :: String -> Condition
getCond str = fromRight undefined (parse condtionP "" str)

main :: IO ()
main =
  hspec $ do
    describe "contain works" $ do
      it "returns false when the output does not contain str" $ do
        getCond "contain(\"bye\")" "hello world" `shouldBe` False
      it "returns true when the output contains str" $ do
        getCond "contain(\"hello\")" "hello world" `shouldBe` True
    describe "be_empty works" $ do
      it "returns false when the output is not empty" $ do
        getCond "be_empty()" "hello world" `shouldBe` False
      it "returns true when the output is empty" $ do
        getCond "be_empty()" "" `shouldBe` True
    describe "have_len" $ do
      it "returns false when the output is not of length len" $ do
        getCond "have_len(\"1\")" "aa" `shouldBe` False
      it "returns true when the output is of length len" $ do
        getCond "have_len(\"1\")" "a" `shouldBe` True
    describe "not" $ do
      it "returns false when the condition is true" $ do
        getCond "not(\"have_len(\\\"1\\\")\")" "a" `shouldBe` False
      it "returns true when the condition is false" $ do
        getCond "not(\"have_len(\\\"1\\\")\")" "aa" `shouldBe` True
    describe "have_prefix" $ do
      it "returns false when the output does not begin with prefix" $ do
        getCond "have_prefix(\"bye\")" "hello world bye!" `shouldBe` False
      it "retruns true when the output does begin with prefix" $ do
        getCond "have_prefix(\"hello\")" "hello world" `shouldBe` True
    describe "have_suffix" $ do
      it "returns false when the output ends with suffix" $ do
        getCond "have_suffix(\"hello\")" "hello world" `shouldBe` False
      it "returns true when the output ends with suffix" $ do
        getCond "have_suffix(\"world\")" "hello world" `shouldBe` True
    describe "be" $ do
      it "returns false if the strings are not the same" $ do
        getCond "be(\"hello world\")" "world hello" `shouldBe` False
      it "returns true if the strings are the same" $ do
        getCond "be(\"hello world\")" "hello world" `shouldBe` True
