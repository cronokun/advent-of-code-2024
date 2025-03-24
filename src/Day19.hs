{-# LANGUAGE OverloadedStrings #-}

-- Day 19: Linen Layout
module Day19 (part1, part2) where

import Data.Maybe (fromJust, isJust)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

-- Returns number of possible towel designs.
part1 :: String -> Int
part1 input =
  let (patterns, designs) = parse input
      patterns' = reverse $ L.sortOn T.length patterns 
   in length . filter (> 0) . map (possiblePatternsFor patterns') $ designs

-- Returns sum of number of different ways you could make each design.
part2 :: String -> Int
part2 input =
  let (patterns, designs) = parse input
      patterns' = reverse $ L.sortOn T.length patterns 
   in sum . map (possiblePatternsFor patterns') $ designs

possiblePatternsFor :: [T.Text] -> T.Text -> Int
possiblePatternsFor patterns design = helper [(design, 1)]
  where
    helper xs = case M.toList $ doStrip M.empty xs of
                  [] -> 0
                  [("", n)] -> n
                  xs' -> helper xs'

    doStrip acc [] = acc
    doStrip acc (("", k) : xs) = doStrip (M.insert "" k acc) xs
    doStrip acc ((d, k) : xs) =
      let striped = [(fromJust s, k) | p <- patterns, let s = T.stripPrefix p d, isJust s]
          acc' = foldr (\(key, val) m -> M.insertWith (+) key val m) acc striped
      in doStrip acc' xs

parse :: String -> ([T.Text], [T.Text])
parse input =
  let [ps, ds] = T.splitOn "\n\n" (T.pack input)
      patterns = T.splitOn ", " ps
      designs = T.lines ds
   in (patterns, designs)
