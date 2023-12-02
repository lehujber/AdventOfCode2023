module Main where

import Data.Char (digitToInt, isDigit)
import Data.List.Split (splitOn)
import Data.Text (Text, pack, replace, unpack)

path = "inp.txt"

stringNumbers :: [String] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

stringNumberMap :: [(String, Int)] = zip stringNumbers [1 ..]

main = do
  input <- readFile path
  let values = init $ splitOn "\n" input

  print $ sum $ map (calibrationValue . digits) values
  print $ sum $ map (calibrationValue . digits . processNumbers) values

digits :: String -> String
digits = filter isDigit

calibrationValue :: String -> Int
calibrationValue s = (\x -> read x :: Int) (head s : [last s])

processNumbers :: String -> String
processNumbers s = unpack $ foldr applyReplace (pack s) stringNumberMap
  where
    applyReplace :: (String, Int) -> Text -> Text
    applyReplace (s, i) = replace (pack s) (pack (s ++ show i ++ s))
