module Day16Spec (spec) where

import Day16.Part1
import Day16.Part2
import Test.Hspec

example1, example2 :: String

example1 =
  "###############\n\
  \#.......#....E#\n\
  \#.#.###.#.###.#\n\
  \#.....#.#...#.#\n\
  \#.###.#####.#.#\n\
  \#.#.#.......#.#\n\
  \#.#.#####.###.#\n\
  \#...........#.#\n\
  \###.#.#####.#.#\n\
  \#...#.....#.#.#\n\
  \#.#.#.###.#.#.#\n\
  \#.....#...#.#.#\n\
  \#.###.#.#.#.#.#\n\
  \#S..#.....#...#\n\
  \###############\n"

example2 =
  "#################\n\
  \#...#...#...#..E#\n\
  \#.#.#.#.#.#.#.#.#\n\
  \#.#.#.#...#...#.#\n\
  \#.#.#.#.###.#.#.#\n\
  \#...#.#.#.....#.#\n\
  \#.#.#.#.#.#####.#\n\
  \#.#...#.#.#.....#\n\
  \#.#.#####.#.###.#\n\
  \#.#.#.......#...#\n\
  \#.#.###.#####.###\n\
  \#.#.#...#.....#.#\n\
  \#.#.#.#####.###.#\n\
  \#.#.#.........#.#\n\
  \#.#.#.#########.#\n\
  \#S#.............#\n\
  \#################\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns the lowest posible score" $ do
      (part1 example1) `shouldBe` 7036
      (part1 example2) `shouldBe` 11048

  describe "part2" $ do
    it "returns number of tiles on the all mix pathes" $ do
      (part2 example1) `shouldBe` 45
      (part2 example2) `shouldBe` 64

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/16_maze_map"
      (part1 input) `shouldBe` 143580

    it "for part 2" $ do
      input <- readFile "inputs/16_maze_map"
      (part2 input) `shouldBe` 645
