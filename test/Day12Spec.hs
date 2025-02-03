module Day12Spec (spec) where

import Day12
import Test.Hspec hiding (example)

example1, example2, example3, example4, example5 :: String

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

example4 = "EEEEE\n\
           \EXXXX\n\
           \EEEEE\n\
           \EXXXX\n\
           \EEEEE\n"

example5 = "AAAAAA\n\
           \AAABBA\n\
           \AAABBA\n\
           \ABBAAA\n\
           \ABBAAA\n\
           \AAAAAA\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total price of fencing all regions" $ do
      (part1 example1) `shouldBe` 140
      (part1 example2) `shouldBe` 772
      (part1 example3) `shouldBe` 1930

  describe "part2" $ do
    it "returns total price of fencing all regions with bulk discount" $ do
      (part2 example1) `shouldBe` 80
      (part2 example2) `shouldBe` 436
      (part2 example3) `shouldBe` 1206
      (part2 example4) `shouldBe` 236
      (part2 example5) `shouldBe` 368

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/12_garden_plots_map"
      (part1 input) `shouldBe` 1473408

    it "for part 2" $ do
      input <- readFile "inputs/12_garden_plots_map"
      (part2 input) `shouldBe` 886364
