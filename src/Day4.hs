-- Day 4: Ceres Search
module Day4 (part1, part2) where

import Data.List (intersect, transpose)

part1 :: String -> Int
part1 input =
  let input' = lines input
   in countHorz input' + countVert input' + countDia1 input' + countDia2 input'

part2 :: String -> Int
part2 input =
  let input' = lines input
      len = length input'
      dia1 = diagonalTranspose dindex1 input'
      dia2 = diagonalTranspose dindex2 input'
      as = map (diaToN1 len) $ allAs dia1
      bs = map (diaToN2 len) $ allAs dia2
  in countIndexMatches as bs
  where
    allAs :: [String] -> [(Int, Int)]
    allAs lst =
      let lstWithIndex = zip [0..] lst
       in concatMap allAsWithIndex lstWithIndex
      where
        allAsWithIndex (line, str) = zip (repeat line) (scanForIndexes str)

    countIndexMatches :: [Int] -> [Int] -> Int
    countIndexMatches as bs = length $ intersect as bs

    scanForIndexes :: String -> [Int]
    scanForIndexes str = helper 0 [] str
      where
        helper _ acc "" = acc
        helper _ acc (_:_:[]) = acc 
        -- add 1 to index because we match 'A' in "MAS" or "SAM"
        helper i acc ('M':'A':'S':rest) = helper (i + 1) ((i + 1) : acc) ('A':'S':rest)
        helper i acc ('S':'A':'M':rest) = helper (i + 1) ((i + 1) : acc) ('A':'M':rest)
        helper i acc (_:rest) = helper (i + 1) acc rest

    diaToN1 :: Int -> (Int, Int) -> Int
    diaToN1 len (l, c)
      | l < len = c * len + (l - c)
      | l >= len = 
        let m = len - (c + 1)
            k = l - m
         in len * k + m

    diaToN2 ::  Int -> (Int, Int) -> Int
    diaToN2 len (l, c)
      | l < len = (len + 1) * c + len - 1 - l
      | l >= len =
        let k = (l - (len - 1))
            m = len * c + c
         in len * k + m

countDia1 :: [String] -> Int
countDia1 input =
  let input' = diagonalTranspose dindex1 input
      input'' = map reverse input'
   in doCount input' + doCount input''


countDia2 :: [String] -> Int
countDia2 input =
  let input' = diagonalTranspose dindex2 input
      input'' = map reverse input'
   in doCount input' + doCount input''

countHorz :: [String] -> Int
countHorz input =
  let input' = map reverse input
   in doCount input + doCount input'

countVert :: [String] -> Int
countVert input =
  let input' = transpose input
      input'' = map reverse input'
   in doCount input' + doCount input''

doCount :: [String] -> Int
doCount lst = foldr (\l t -> t + count' 0 l) 0 lst
  where
    count' :: Int -> String -> Int
    count' acc "" = acc
    count' acc ('X':'M':'A':'S':rest) = count' (acc + 1) rest
    count' acc (_:rest) = count' acc rest

diagonalTranspose :: (Int -> Int -> Int) -> [String] -> [String]
diagonalTranspose f lst =
  let chars = concat lst
      len = length lst
      acc = replicate (len * 2 - 1) ""
   in diagonalize 0 acc len f chars
  where
    diagonalize _ acc _ _ "" = map reverse acc
    diagonalize n acc len f (x:xs) =
        let idx = f n len
            acc' =  prependAt idx x acc
         in diagonalize (n + 1) acc' len f xs

    prependAt i el list =
      let (head', (row:tail')) = splitAt i list
          row' = [el:row]
       in concat [head', row', tail']

dindex1 :: Int -> Int -> Int
dindex1 n len =  n `div` len + n `rem` len

dindex2 :: Int -> Int -> Int
dindex2 n len =
  let k = n `div` len
      m = n `rem` len
   in (len - 1 + k) - m
