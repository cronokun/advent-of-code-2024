module Day11Spec (spec) where

import Day11
import Test.Hspec hiding (example)

example :: String
example = "125 17"

spec :: Spec
spec = do
  describe "part1" $ do
    it "Returns number of stones after blinking N times" $ do
      (part1 25 example) `shouldBe` 55312

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/11_stones_arrangement"
      (part1 25 input) `shouldBe` 185894
