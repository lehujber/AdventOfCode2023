module Day04 where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

path = "inp.txt"

type Card = ([Int], [Int])

type CardMap = [(Card, [Card])]

main = do
  input <- readFile path
  let cards = map ((\[f, s] -> (f, s)) . map (map (\x -> read x :: Int) . words) . splitOn " | " . drop 2 . dropWhile (/= ':')) $ lines input
  print $ sum $ map cardValue cards
  print $ sum $ map (`processCard` cards) cards

validLength :: Card -> Int
validLength (w, v) = length $ filter (`elem` w) v

cardValue :: Card -> Int
cardValue c = calcVal $ validLength c
  where
    calcVal :: Int -> Int
    calcVal n
      | n <= 0 = 0
      | otherwise = 2 ^ (n - 1)

cardWinMap :: [Card] -> CardMap
cardWinMap [] = []
cardWinMap (c : cs) = (c, take (validLength c) cs) : cardWinMap cs

getFromMap :: Card -> CardMap -> [Card]
getFromMap c cm = snd $ fromMaybe (([], []), []) (find (\(x, _) -> x == c) cm)

processCard :: Card -> [Card] -> Int
processCard card cards = process [(1, card)] 0 (cardWinMap cards)
  where
    process :: [(Int, Card)] -> Int -> CardMap -> Int
    process [] acc _ = acc
    process ((n, c) : cs) acc map = process (newCards (n, c) cs map) (acc + n) map

    newCards :: (Int, Card) -> [(Int, Card)] -> CardMap -> [(Int, Card)]
    newCards (n, c) cs map = foldr (\x -> addCard (n, x)) cs (getFromMap c map)

    addCard :: (Int, Card) -> [(Int, Card)] -> [(Int, Card)]
    addCard (n, c) [] = [(n, c)]
    addCard (ne, ce) ((nl, cl) : l)
      | ce == cl = (ne + nl, ce) : l
      | otherwise = (nl, cl) : addCard (ne, ce) l
