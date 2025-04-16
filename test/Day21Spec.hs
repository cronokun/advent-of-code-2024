module Day21Spec (spec) where

import Day21
import Test.Hspec hiding (example)

example :: String
example = "029A\n\
          \980A\n\
          \179A\n\
          \456A\n\
          \379A\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total complexity of all codes" $ do
      part1 example `shouldBe` 126384

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/21_door_codes"
      part1 input `shouldBe` 219254
