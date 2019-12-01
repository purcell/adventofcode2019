module Day01 where

import qualified Parse as P
import Parse (Parser)


fuelRequired :: Int -> Int
fuelRequired mass = mass `div` 3 - 2

fullFuelRequired = sum . takeWhile (>= 0) . tail . iterate fuelRequired

day1a = sum . fmap fuelRequired

day1b = sum . fmap fullFuelRequired

numbers :: Parser [Int]
numbers = P.many (P.decimal <* P.newline)

day1 :: IO ()
day1 = do
  putStrLn "Day 1"
  vals <- P.parseFile numbers "input/01.txt"
  putStrLn "A:"
  print (day1a vals)
  putStrLn "B:"
  print (day1b vals)

