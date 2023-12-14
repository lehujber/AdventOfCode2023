module Day04 where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

path = "inp2.txt"

type Card = ([Int], [Int])

type CardMap = [(Card, [Card])]

main = do
  input <- readFile path
  let cards = map ((\[f, s] -> (f, s)) . map (map (\x -> read x :: Int) . words) . splitOn " | " . drop 2 . dropWhile (/= ':')) $ lines input
  print $ sum $ map cardValue cards
  print cards
  print $ wonCards (head cards) cards

validLength :: Card -> Int
validLength (w, v) = length $ filter (`elem` w) v

cardValue :: Card -> Int
cardValue c = calcVal $ validLength c
  where
    calcVal :: Int -> Int
    calcVal n
      | n <= 0 = 0
      | otherwise = 2 ^ (n - 1)

wonCards :: Card -> [Card] -> [Card]
wonCards c l = take (validLength c) $ tail $ dropWhile (/= c) l

cardWinMap :: [Card] -> CardMap
cardWinMap l = map (\x -> (x, wonCards x l)) l

getFromMap :: Card -> CardMap -> [Card]
getFromMap c cm = snd $ fromMaybe (([], []), []) (find (\(x, _) -> x == c) cm)
