module Day22Spec (spec) where

import Day22
import Test.Hspec hiding (example)

example :: String
example = "1\n10\n100\n2024"

spec :: Spec
spec = do
  describe "part1" $ do
    it "return sum of 2000th secret numbers" $ do
      part1 example `shouldBe` 37327623

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/22_secret_numbers"
      part1 input `shouldBe` 20506453102
