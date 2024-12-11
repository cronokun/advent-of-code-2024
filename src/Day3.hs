-- description: Day 3: Mull It Over

module Day3 (part1, part2) where

type Pair = (Integer, Integer)

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

-- Part 1

parse :: String -> [Pair]
parse input = parse' [] input
  where
    parse' acc ('m':'u':'l':'(':str) = maybeParsePair acc str
    parse' acc "" = acc
    parse' acc (_:str) = parse' acc str

    maybeParsePair acc str =
      case parsePair str of
        Left rest  -> parse' acc rest
        Right (pair, rest) -> parse' (pair : acc) rest

-- Part 2

parseEnabled :: String -> [Pair]
parseEnabled input = parse' True [] input
  where
    parse' _ acc ('d':'o':'(':')':rest) = parse' True acc rest
    parse' _ acc ('d':'o':'n':'\'':'t':'(':')':rest) = parse' False acc rest
    parse' True acc ('m':'u':'l':'(':str) = maybeParsePair acc str
    parse' _ acc "" = acc
    parse' enabled acc (_:str) = parse' enabled acc str

    maybeParsePair acc str =
      case parsePair str of
        Left rest  -> parse' True acc rest
        Right (pair, rest) -> parse' True (pair : acc) rest

addUpMultiplications :: [Pair] -> Integer
addUpMultiplications pairs = foldr (+) 0 . map (uncurry (*)) $ pairs

part1 :: String -> Integer
part1 input = addUpMultiplications $ parse input

part2 :: String -> Integer
part2 input = addUpMultiplications $ parseEnabled input
