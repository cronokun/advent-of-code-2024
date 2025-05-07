-- Day 25: Code Chronicle
module Day25 (part1) where

import qualified Data.List as L
import Helpers (lineGroups)

data Scheme = Key | Lock
type Pins = [Int]

-- Returns number of uniq fitting lock/key pairs.
part1 :: String -> Int
part1 input =
  let (ks, ls) = parse input
   in run 0 ls ks
  where
    run acc _ [] = acc
    run acc ls (k:ks) =
      let n = length $ filter (isFit k) ls
       in run (acc + n) ls ks

    isFit :: Pins -> Pins -> Bool
    isFit [] [] = True
    isFit (k:ks) (l:ls) =
      if k + l <= 7
         then isFit ks ls
         else False

parse :: String -> ([Pins], [Pins])
parse input = parseGroups [] [] $ lineGroups input
  where
    parseGroups :: [Pins] -> [Pins] -> [[String]] -> ([Pins], [Pins])
    parseGroups ks ls [] = (ks, ls)
    parseGroups ks ls (x:xs) =
      case schemeType of
        Key -> parseGroups (keyPins x : ks) ls xs
        Lock -> parseGroups ks (lockPins x : ls) xs
      where
        schemeType = if head x == "#####" then Lock else Key
        keyPins :: [String] -> Pins
        keyPins = map (length . L.dropWhile (== '.')) . L.transpose
        lockPins :: [String] -> Pins
        lockPins = map (length . L.takeWhile (== '#')) . L.transpose
