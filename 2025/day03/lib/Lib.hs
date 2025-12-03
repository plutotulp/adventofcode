module Lib where

import System.Environment qualified as Environment
import qualified Data.List as List

type Bank = [Int]

parse :: String -> [Bank]
parse = fmap (fmap (read . pure)) . lines

-- Assumes we have at least two values in Bank
joltage1 :: Bank -> Int
joltage1 xs = 10 * d1 + d2
  where
    d1 = maximum (take (length xs - 1) xs)
    rs = drop 1 $ dropWhile (/= d1) xs
    d2 = maximum rs


joltage2 :: Int -> Bank -> Int
joltage2 nd0 = go nd0 0
  where
    go 0 v _  = v
    go n v ds =
      let
        -- Look for highest digit we could use while leaving space for
        -- the remaining n digits.
        cand = take (1 + length ds - n) ds
        d = maximum cand
        ds' = drop 1 $ List.dropWhile (/= d) ds
      in
        go (n - 1) (v * 10 + d) ds'


solve :: [Bank] -> IO ()
solve bs = do
  putStrLn $
    "Part 1: Total output joltage is " ++ show (sum $ joltage1 <$> bs)
  putStrLn $
    "Part 1: New total output joltage is " ++ show (sum $ joltage2 12 <$> bs)

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- readFile inputFile
  Lib.solve $ Lib.parse input
