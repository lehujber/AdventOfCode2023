module Main where

import Data.Char (digitToInt)
import Data.List.Split (splitOn)

path = "inp.txt"

type Turn = (Int, Int, Int)

type Game = [Turn]

parseTurn :: [String] -> Turn
parseTurn l = toTurn (map (splitOn " ") l) (0, 0, 0)
  where
    toTurn :: [[String]] -> Turn -> Turn
    toTurn [] acc = acc
    toTurn ([v, c] : l) (r, g, b)
      | c == "red" = toTurn l (r + (read v :: Int), g, b)
      | c == "green" = toTurn l (r, g + (read v :: Int), b)
      | c == "blue" = toTurn l (r, g, b + (read v :: Int))
    toTurn _ acc = acc

main = do
  input <- readFile path
  let games = zip [1 ..] $ map (map (parseTurn . splitOn ", ") . splitOn "; " . drop 2 . dropWhile (/= ':')) $ init $ splitOn "\n" input
  print $ foldr (\(f, _) acc -> acc + f) 0 $ filter (\(f, s) -> all validTurn s) games
  print $ sum $ map ((\(r, g, b) -> r * g * b) . minCubes . snd) games

validTurn :: Turn -> Bool
validTurn (r, g, b) = r <= 12 && g <= 13 && b <= 14

minCubes :: [Turn] -> Turn
minCubes = foldr (\(ri, bi, gi) (ra, ba, ga) -> (max ra ri, max ba bi, max ga gi)) (0, 0, 0)
