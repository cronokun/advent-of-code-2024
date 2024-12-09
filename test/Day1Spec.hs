module Day1Spec (spec) where

import Test.Hspec
import Day1
import System.IO.Unsafe (unsafePerformIO)

sample :: String
sample = "3   4\n\
           \4   3\n\
           \2   5\n\
           \1   3\n\
           \3   9\n\
           \3   3"

input :: String
input = unsafePerformIO . readFile $ "inputs/1_locations_list"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total distance between lists" $ do
      shouldBe 11 (part1 sample)

  describe "part2" $ do
    it "returns total distance between lists" $ do
      shouldBe 31 (part2 sample)

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe 2192892 (part1 input)

    it "for part 2" $ do
      shouldBe 22962826 (part2 input)
