module Day12Spec (spec) where

import Day12
import Test.Hspec hiding (example)

example1, example2, example3 :: String

example1 = "AAAA\n\
           \BBCD\n\
           \BBCC\n\
           \EEEC\n"

example2 = "OOOOO\n\
           \OXOXO\n\
           \OOOOO\n\
           \OXOXO\n\
           \OOOOO\n"

example3 = "RRRRIICCFF\n\
           \RRRRIICCCF\n\
           \VVRRRCCFFF\n\
           \VVRCCCJFFF\n\
           \VVVVCJJCFE\n\
           \VVIVCCJJEE\n\
           \VVIIICJJEE\n\
           \MIIIIIJJEE\n\
           \MIIISIJEEE\n\
           \MMMISSJEEE\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total price of fencing all regions" $ do
      (part1 example1) `shouldBe` 140
      (part1 example2) `shouldBe` 772
      (part1 example3) `shouldBe` 1930

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/12_garden_plots_map"
      (part1 input) `shouldBe` 1473408
