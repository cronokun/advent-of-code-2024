module Day10Spec (spec) where

import Day10
import Test.Hspec hiding (example)

example :: String
example = "89010123\n\
          \78121874\n\
          \87430965\n\
          \96549874\n\
          \45678903\n\
          \32019012\n\
          \01329801\n\
          \10456732\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "Returns sum of trailhead scores" $ do
      (part1 example) `shouldBe` 36

  describe "part2" $ do
    it "Returns sum of trailhead ratings" $ do
      (part2 example) `shouldBe` 81

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/10_topographic_map"
      (part1 input) `shouldBe` 510

    it "for part 2" $ do
      input <- readFile "inputs/10_topographic_map"
      (part2 input) `shouldBe` 1058
