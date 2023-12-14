module Day04 where

import Data.List.Split (splitOn)

path = "inp.txt"

type Card = ([Int], [Int])

main = do
  input <- readFile path
  let values = map ((\[f, s] -> (f, s)) . map (map (\x -> read x :: Int) . words) . splitOn " | " . drop 2 . dropWhile (/= ':')) $ lines input
  print $ sum $ map cardValue values

cardValue :: Card -> Int
cardValue (w, v) = calcVal validLength
  where
    validLength :: Int
    validLength = length $ filter (`elem` w) v
    calcVal :: Int -> Int
    calcVal n
      | n <= 0 = 0
      | otherwise = 2 ^ (n - 1)
