module Day17Spec (spec) where

import Day17
import Test.Hspec hiding (example)

example :: String
example = "Register A: 729\n\
          \Register B: 0\n\
          \Register C: 0\n\
          \\n\
          \Program: 0,1,5,4,3,0\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "Returns the output debug string" $ do
      (part1 example) `shouldBe` "4,6,3,5,6,3,5,2,1,0"

  describe "answers" $ do
    it "for part1" $ do
      input <- readFile "inputs/17_program_info"
      (part1 input) `shouldBe` "1,3,5,1,7,2,5,1,6"
