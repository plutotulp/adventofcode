module Lib (parse, solve) where

import Data.Bifunctor qualified as Bifunctor
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map
import Data.IntSet ( IntSet )
import Data.IntSet qualified as IntSet
import Control.Monad.State ( State )
import Control.Monad.State qualified as State

type Stone = Word

parse :: String -> [Stone]
parse = concatMap ((read @Word <$>) . words) . lines

evenNumDigits :: Word -> Bool
evenNumDigits = even . length . show

blinkStone :: Stone -> Either Stone (Stone, Stone)
blinkStone s
  | s == 0 = Left 1
  | evenNumDigits s = Right $ split s
  | otherwise = Left $ s * 2024

split :: Stone -> (Stone, Stone)
split s =
  -- note that `read` parses 0-prefixed numbers as normal decimal,
  -- not octal, so no need to trim away zeroes on the right half
  Bifunctor.bimap read read $ splitAt (n `div` 2) ds
  where
    ds = show s
    n  = length ds

naiveBlink :: [Stone] -> [Stone]
naiveBlink = foldr step []
  where
    step s acc = either (: acc) (prefixPair acc) $ blinkStone s
    prefixPair acc (a, b) = a : b : acc

-- type KnownBlinks = Map (Stone, Word) Word

-- cachedBlinkStone :: KnownBlinks -> Stone

-- blinkLen :: Stone -> Int
-- blinkLen

solve :: [Stone] -> IO ()
solve ss = do
  putStrLn $
    "Part 1: after blinking 25 times you have "
    ++ show (length $ last $ take 26 $ iterate naiveBlink ss)
    ++ " stones"
  -- putStrLn $
  --   "Part 1: after blinking 75 times you have "
  --   ++ show (length $ last $ take 76 $ iterate naiveBlink ss)
  --   ++ " stones"
  mapM_ print $ take 10 $ iterate naiveBlink ss
  mapM_ print $ take 10 (IntSet.fromList . fmap fromIntegral <$> iterate naiveBlink ss)
