module Day2Spec (spec) where

import Day2
import Test.Hspec

sample :: String
sample = "7 6 4 2 1\n\
         \1 2 7 8 9\n\
         \9 7 6 2 1\n\
         \1 3 2 4 5\n\
         \8 6 4 4 1\n\
         \1 3 6 7 9"


spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of safe reports" $ do
      shouldBe (part1 sample) 2

  describe "part2" $ do
    it "returns number of safe reports with one-off tolerance" $ do
      shouldBe (part2 sample) 4

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/2_unusual_data"
      shouldBe (part1 input) 663

    it "for part 2" $ do
      input <- readFile "inputs/2_unusual_data"
      shouldBe (part2 input) 692
