module Day01 where

import qualified Parse as P
import Parse (Parser)


fuelRequired :: Int -> Int
fuelRequired mass = mass `div` 3 - 2


numbers :: Parser [Int]
numbers = P.many (P.decimal <* P.newline)



day1 :: IO Int
day1 = do
  vals <- P.parseFile numbers "input/01.txt"
  pure $ sum $ fuelRequired <$> vals
