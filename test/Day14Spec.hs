module Day14Spec (spec) where

import Day14
import Test.Hspec hiding (example)

example :: String
example = "p=0,4 v=3,-3\n\
          \p=6,3 v=-1,-3\n\
          \p=10,3 v=-1,2\n\
          \p=2,0 v=2,-1\n\
          \p=0,0 v=1,3\n\
          \p=3,0 v=-2,-2\n\
          \p=7,6 v=-1,-3\n\
          \p=3,0 v=-1,-2\n\
          \p=9,3 v=2,3\n\
          \p=7,3 v=-1,2\n\
          \p=2,4 v=2,-3\n\
          \p=9,5 v=-3,-3\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns safety factor after 100 seconds" $ do
      (part1 (11, 7) example) `shouldBe` 12

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/14_robot_records"
      (part1 (101, 103) input) `shouldBe` 232589280

    it "for part 2" $ do
      input <- readFile "inputs/14_robot_records"
      (part2 (101, 103) input) `shouldBe` 7569
