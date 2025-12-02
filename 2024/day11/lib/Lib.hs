module Lib (parse, solve) where

import Data.Bifunctor qualified as Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

type Stone = Int

parse :: String -> [Stone]
parse = concatMap ((read <$>) . words) . lines

evenNumDigits :: Stone -> Bool
evenNumDigits = even . length . show

blinkStone :: Stone -> Either Stone (Stone, Stone)
blinkStone s
  | s == 0 = Left 1
  | evenNumDigits s = Right $ split s
  | otherwise = Left $ s * 2024

split :: Stone -> (Stone, Stone)
split s =
  -- Note that `read` parses 0-prefixed numbers as normal decimal, not
  -- octal, so no need to trim away zeroes on the right half.
  Bifunctor.bimap read read $ splitAt (n `div` 2) ds
  where
    ds = show s
    n = length ds

naiveBlink :: [Stone] -> [Stone]
naiveBlink = foldr step []
  where
    step s acc = either (: acc) (prefixPair acc) $ blinkStone s
    prefixPair acc (a, b) = a : b : acc

-- Map from stone to the number of copies we have of that stone
type StoneCount = IntMap Int

mkStoneCount :: [Stone] -> StoneCount
mkStoneCount = IntMap.fromListWith (+) . fmap (,1)

stoneCountTotal :: StoneCount -> Int
stoneCountTotal = sum . IntMap.elems

blink :: StoneCount -> StoneCount
blink mp = IntMap.foldrWithKey step mp mp
  where
    step s0 n0 m0 =
      IntMap.alter (del n0) s0 $
        either
          (\s1 -> IntMap.alter (add n0) s1 m0)
          (\(s1, s2) -> IntMap.alter (add n0) s1 $ IntMap.alter (add n0) s2 m0)
          (blinkStone s0)

    add n0 Nothing = Just n0
    add n0 (Just v) = Just $ v + n0

    del _ Nothing =
      -- Does not happen, as the stone we're trying to remove (s0) was
      -- fed to step from the map we're trying to remove it from.
      error "tried to remove already absent stone"
    del n0 (Just v) =
      let v' = v - n0
       in if v' == 0 then Nothing else Just v'-- | Apply a function @n@ times to a given value.

times :: Int -> (a -> a) -> a -> a
times n f x =
  case drop n $ iterate f x of
    res : _ -> res
    [] ->
      -- This pattern match is here to avoid using `head`, which the
      -- compiler does not like very much.
      error "unreachable, as f always produces new results"

solve :: [Stone] -> IO ()
solve ss = do
  putStrLn $
    "Part 1: after blinking 25 times you have "
      ++ show (length $ times 25 naiveBlink ss)
      ++ " stones"
  putStrLn $
    "Part 1: after blinking 75 times you have "
      ++ show (stoneCountTotal $ times 75 blink $ mkStoneCount ss)
      ++ " stones"
