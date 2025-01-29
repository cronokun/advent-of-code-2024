module Day11Spec (spec) where

import Day11
import Test.Hspec hiding (example)

example :: String
example = "125 17"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of stones after blinking 25 times" $ do
      (part1 example) `shouldBe` 55312

  describe "part1" $ do
    it "returns number of stones after blinking 75 times" $ do
      (part2 example) `shouldBe` 65601038650482

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/11_stones_arrangement"
      (part1 input) `shouldBe` 185894

    it "for part 2" $ do
      input <- readFile "inputs/11_stones_arrangement"
      (part2 input) `shouldBe` 221632504974231
