-- description: Day 3: Mull It Over

module Day3 (part1) where

type Pair = (Integer, Integer)

parse :: String -> [Pair]
parse input = parse' [] input
  where
    parse' acc ('m':'u':'l':'(':str) = maybeParse acc str
    parse' acc "" = acc
    parse' acc (_:str) = parse' acc str

    maybeParse acc str =
      case parsePair str of
        Left rest  -> parse' acc rest
        Right (pair, rest) -> parse' (pair : acc) rest

parsePair :: String -> Either String (Pair, String)
parsePair str =
  case parseNum str of
    Left rest -> Left rest
    Right (a, rest) ->
      case parseNum rest of
        Left rest' -> Left rest'
        Right (b, rest'') -> Right ((a, b), rest'')

parseNum :: String -> Either String (Integer, String)
parseNum = parseNum' ""
  where
    parseNum' acc (x:rest)
      | x `elem` ['0'..'9'] = parseNum' (x:acc) rest
      | x `elem` [',', ')'] =
        let num = (read . reverse $ acc :: Integer)
         in Right (num, rest)
      | otherwise = Left rest

part1 :: String -> Integer
part1 input =
  let addUp = foldr (+) 0 
      multiply = map (uncurry (*))
   in addUp . multiply $ parse input
