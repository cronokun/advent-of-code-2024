module Day17Spec (spec) where

import Day17
import Test.Hspec

example1, example2 :: String

example1 = "Register A: 729\n\
          \Register B: 0\n\
          \Register C: 0\n\
          \\n\
          \Program: 0,1,5,4,3,0\n"

example2 = "Register A: 2024\n\
           \Register B: 0\n\
           \Register C: 0\n\
           \\n\
           \Program: 0,3,5,4,3,0\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "Returns the output debug string" $ do
      (part1 example1) `shouldBe` "4,6,3,5,6,3,5,2,1,0"

  describe "part2" $ do
    it "Returns lowest positive initial value for register A that make program to output a copy of itself" $ do
      (part2 example2) `shouldBe` 117440

  describe "answers" $ do
    it "for part1" $ do
      input <- readFile "inputs/17_program_info"
      (part1 input) `shouldBe` "1,3,5,1,7,2,5,1,6"

    it "for part2" $ do
      input <- readFile "inputs/17_program_info"
      (part2 input) `shouldBe` 236555997372013
