module Day15Spec (spec) where

import Day15
import Test.Hspec

example1, example2 :: String

example1 = "########\n\
           \#..O.O.#\n\
           \##@.O..#\n\
           \#...O..#\n\
           \#.#.O..#\n\
           \#...O..#\n\
           \#......#\n\
           \########\n\
           \\n\
           \<^^>>>vv<v>>v<<\n"

example2 =
  "##########\n\
  \#..O..O.O#\n\
  \#......O.#\n\
  \#.OO..O.O#\n\
  \#..O@..O.#\n\
  \#O#..O...#\n\
  \#O..O..O.#\n\
  \#.OO.O.OO#\n\
  \#....O...#\n\
  \##########\n\
  \\n\
  \<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
  \vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
  \><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
  \<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
  \^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
  \^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
  \>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
  \<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
  \^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
  \v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n"

spec :: Spec
spec = do
  describe "part1" $ do
    it "returns sum of all boxes' GPS coordinates" $ do
      (part1 example1) `shouldBe` 2028
      (part1 example2) `shouldBe` 10092

  describe "answers" $ do
    it "for part 1" $ do
      input <- readFile "inputs/15_map_and_moves"
      (part1 input) `shouldBe` 1412971
