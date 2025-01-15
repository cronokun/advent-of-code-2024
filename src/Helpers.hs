module Helpers (inspect, splitOn) where

import Debug.Trace (trace)

inspect :: Show a => String -> a -> a
inspect msg res = trace (msg ++ ": " ++ show res) res

-- Split a string into two on the specified char
splitOn :: Char -> String -> [String]
splitOn p str = doSplit [] [] str
  where
    doSplit as acc "" = reverse ((reverse as) : acc)
    doSplit as acc (x:xs)
      | p == x = doSplit [] ((reverse as) : acc) xs
      | otherwise = doSplit (x:as) acc xs
