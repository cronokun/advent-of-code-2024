module Day25Spec (spec) where

import Day25
import Test.Hspec hiding (example)

example :: String
example = "#####\n\
          \.####\n\
          \.####\n\
          \.####\n\
          \.#.#.\n\
          \.#...\n\
          \.....\n\
          \\n\
          \#####\n\
          \##.##\n\
          \.#.##\n\
          \...##\n\
          \...#.\n\
          \...#.\n\
          \.....\n\
          \\n\
          \.....\n\
          \#....\n\
          \#....\n\
          \#...#\n\
          \#.#.#\n\
          \#.###\n\
          \#####\n\
          \\n\
          \.....\n\
          \.....\n\
          \#.#..\n\
          \###..\n\
          \###.#\n\
          \###.#\n\
          \#####\n\
          \\n\
          \.....\n\
          \.....\n\
          \.....\n\
          \#....\n\
          \#.#..\n\
          \#.#.#\n\
          \#####\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns number of uniq fitting lock/key pairs" $ do
      part1 example `shouldBe` 3

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/25_locks_and_keys"
      part1 input `shouldBe` 3483
