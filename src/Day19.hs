{-# LANGUAGE OverloadedStrings #-}

-- Day 19: Linen Layout
module Day19 (part1, part2) where

import Data.Maybe (fromJust, isJust)
import qualified Data.List as L
import qualified Data.Map as M
import Helpers (lineGroups, splitOnL)

-- Returns number of possible towel designs.
part1 :: String -> Int
part1 input =
  let (patterns, designs) = parse input
   in length . filter (isPossible patterns) $ designs
  where
    isPossible :: [String] -> String -> Bool
    isPossible patterns design = helper [design]
      where
        helper xs =
          case stripPatternsFrom xs of
            [] -> False
            rs -> if any (== "") rs then True else helper rs

        stripPatternsFrom :: [String] -> [String]
        stripPatternsFrom xs = L.nub [fromJust s | d <- xs, p <- patterns, let s = L.stripPrefix p d, isJust s]

-- Returns sum of number of different ways you could make each design.
part2 :: String -> Int
part2 input =
  let (patterns, designs) = parse input
   in sum . map (possiblePatternsFor patterns) $ designs
  where
    possiblePatternsFor :: [String] -> String -> Int
    possiblePatternsFor patterns design = helper [(design, 1)]
      where
        helper xs = case doStrip M.empty xs of
                      [] -> 0
                      [("", n)] -> n
                      xs' -> helper xs'

        doStrip :: M.Map String Int -> [(String, Int)] -> [(String, Int)]
        doStrip acc [] = M.toList acc
        doStrip acc (("", k) : xs) = doStrip (M.insert "" k acc) xs
        doStrip acc ((d, k) : xs) =
          let striped = [(fromJust s, k) | p <- patterns, let s = L.stripPrefix p d, isJust s]
              acc' = foldr (\(key, val) m -> M.insertWith (+) key val m) acc striped
          in doStrip acc' xs

parse :: String -> ([String], [String])
parse input =
  let [[ps], designs] = lineGroups input
      patterns = splitOnL ", " ps
   in (patterns, designs)
