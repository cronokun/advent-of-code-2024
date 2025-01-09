module Day1Spec (spec) where

import Day1
import Test.Hspec
import SpecHelper

example1 :: String
example1 = "3   4\n\
           \4   3\n\
           \2   5\n\
           \1   3\n\
           \3   9\n\
           \3   3"

input :: String
input = readFile' "inputs/1_locations_list"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total distance between lists" $ do
      shouldBe (part1 example1) 11

  describe "part2" $ do
    it "returns total distance between lists" $ do
      shouldBe (part2 example1) 31

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe (part1 input) 2192892

    it "for part 2" $ do
      shouldBe (part2 input) 22962826
