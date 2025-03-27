module Day20Spec (spec) where

import Day20
import Test.Hspec hiding (example)

example :: String
example = "###############\n\
          \#...#...#.....#\n\
          \#.#.#.#.#.###.#\n\
          \#S#...#.#.#...#\n\
          \#######.#.#.###\n\
          \#######.#.#...#\n\
          \#######.#.###.#\n\
          \###..E#...#...#\n\
          \###.#######.###\n\
          \#...###...#...#\n\
          \#.#####.#.###.#\n\
          \#.#...#.#.#...#\n\
          \#.#.#.#.#.#.###\n\
          \#...#...#...###\n\
          \###############\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns total number of existing cheats" $ do
      part1 example 0 `shouldBe` 44

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/20_racetrack"
      part1 input 100 `shouldBe` 1415
