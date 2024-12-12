-- description: Day 4: Ceres Search

module Day4 (part1) where

import Data.List (transpose)

countDia1 :: [String] -> Integer
countDia1 input =
  let input' = diagonalTranspose input
      input'' = map reverse input'
   in doCount input' + doCount input''


countDia2 :: [String] -> Integer
countDia2 input =
  let input' = diagonalTranspose . transpose . reverse $ input
      input'' = map reverse input'
   in doCount input' + doCount input''

countHorz :: [String] -> Integer
countHorz input =
  let input' = map reverse input
   in doCount input + doCount input'

countVert :: [String] -> Integer
countVert input =
  let input' = transpose input
      input'' = map reverse input'
   in doCount input' + doCount input''

doCount :: [String] -> Integer
doCount lines = foldr (\l t -> t + count' 0 l) 0 lines
  where
    count' :: Integer -> String -> Integer
    count' acc "" = acc
    count' acc ('X':'M':'A':'S':rest) = count' (acc + 1) rest
    count' acc (_:rest) = count' acc rest

diagonalTranspose :: [String] -> [String]
diagonalTranspose lines =
  let chars = concat lines
      len = length . head $ lines
      acc = replicate (len * 2 - 1) ""
   in diagonalize 0 acc len chars
  where
    diagonalize _ acc _ "" = reverse acc
    diagonalize n acc len str@(x:xs) =
        let idx = n `div` len + n `mod` len
            acc' =  prependAt idx x acc
         in diagonalize (n + 1) acc' len xs

    prependAt i el list =
      let (head', (row:tail')) = splitAt i list
          row' = [el:row]
       in concat [head', row', tail']


part1 :: String -> Integer
part1 input =
  let input' = lines input
   in countHorz input' + countVert input' + countDia1 input' + countDia2 input'
