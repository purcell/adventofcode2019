module Day02 where

import qualified Parse as P
import Parse (Parser)
import qualified Data.Vector as V
import Data.List (find)

type OpCode = Int
type Program = V.Vector OpCode

run :: Program -> Program
run = runFrom 0
  where runFrom pos prog =
          case at pos of
            99 -> prog
            1 -> runFrom (pos + 4) $ setAt (at (pos + 3)) (atAt (pos + 1) + atAt (pos + 2))
            2 -> runFrom (pos + 4) $ setAt (at (pos + 3)) (atAt (pos + 1) * atAt (pos + 2))
            _ -> error "wtf"
          where at n = prog V.! n
                atAt = at . at
                setAt n v = prog V.// [(n, v)]


input :: Int -> Int -> Program -> Program
input a b p = p V.// [(1, a), (2, b)]

restore :: Program -> Program
restore = input 12 2

day2a :: Program -> Int
day2a = (V.! 0) . run . restore

day2b :: Program -> Int
day2b prog = 100 * n + v
  where
  Just (n, v) = find (\(n, v) -> 19690720 == run (input n v prog) V.! 0) [(n, v) | n <- [0..99], v <- [0..99]]

program :: Parser Program
program = V.fromList <$> (P.sepBy1 P.decimal (P.char ',') <* P.newline)

day2 :: IO ()
day2 = do
  putStrLn "Day 1"
  prog <- P.parseFile program "input/02.txt"
  putStrLn "A:"
  print (day2a prog)
  putStrLn "B:"
  print (day2b prog)

