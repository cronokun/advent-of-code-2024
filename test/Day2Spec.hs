module Day2Spec (spec) where

import Test.Hspec
import Day2
import System.IO.Unsafe (unsafePerformIO)

sample :: String
sample = "7 6 4 2 1\n\
           \1 2 7 8 9\n\
           \9 7 6 2 1\n\
           \1 3 2 4 5\n\
           \8 6 4 4 1\n\
           \1 3 6 7 9"

input :: String
input = unsafePerformIO . readFile $ "inputs/2_unusual_data"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of safe reports" $ do
      shouldBe 2 (part1 sample)

  describe "part2" $ do
    it "returns number of safe reports with one-off tolerance" $ do
      shouldBe 4 (part2 sample)

  describe "answers" $ do
    it "for part 1" $ do
      shouldBe 663 (part1 input)

    it "for part 2" $ do
      shouldBe 692 (part2 input)
