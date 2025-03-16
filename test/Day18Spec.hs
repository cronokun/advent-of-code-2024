module Day18Spec (spec) where

import Day18
import Test.Hspec hiding (example)

example :: String
example = "5,4\n\
          \4,2\n\
          \4,5\n\
          \3,0\n\
          \2,1\n\
          \6,3\n\
          \2,4\n\
          \1,5\n\
          \0,6\n\
          \3,3\n\
          \2,6\n\
          \5,1\n\
          \1,2\n\
          \5,5\n\
          \2,5\n\
          \6,5\n\
          \1,4\n\
          \0,4\n\
          \6,4\n\
          \1,1\n\
          \6,1\n\
          \1,0\n\
          \0,5\n\
          \1,6\n\
          \2,0\n"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "returns minimum number of steps needed to reach the exit" $ do
      (part1 example 12 (6,6)) `shouldBe` 22

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/18_list_of_bytes"
      (part1 input 1024 (70,70)) `shouldBe` 260
