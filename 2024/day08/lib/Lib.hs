module Lib ( parse, solve )where

import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map
import Data.Set ( Set )
import Data.Set qualified as Set
import qualified Data.List as List

type Col = Int
type Row = Int

newtype Pos = Pos { unPos :: (Row, Col) }
  deriving stock (Eq, Ord, Show)

parse :: String -> Map Pos Char
parse str = Map.fromList entries
  where
    ls = lines str
    entries = concat $ zipWith mkRow [0..] ls
    mkRow ir = zipWith (mkCol ir) [0..]
    mkCol ir ic v = (Pos (ir, ic), v)

getDims :: Map Pos a -> (Int, Int)
getDims mp = (maximum rs, maximum cs)
  where
    ks = Map.keys mp
    rs = fst . unPos <$> ks
    cs = snd . unPos <$> ks

type Antennas = Map Char (Set Pos)
type Antinodes = Set Pos

mkAntennas :: Map Pos Char -> Antennas
mkAntennas = Map.foldlWithKey step mempty
  where
    step as _   '.' = as -- not an antenna
    step as pos ch  = Map.insertWith Set.union ch (Set.singleton pos) as

newtype Off = Off { unOff :: (Row, Col) }
  deriving (Eq, Ord, Show)

posDiff :: Pos -> Pos -> Off
(Pos (r1, c1)) `posDiff` (Pos (r2, c2)) = Off (r1 - r2, c1 - c2)

posAdd :: Pos -> Off -> Pos
posAdd (Pos (r1, c1)) (Off (r2, c2)) = Pos (r1 + r2, c1 + c2)

posIsOnMap :: (Int, Int) -> Pos -> Bool
posIsOnMap (maxR, maxC) (Pos (r, c)) = 0 <= r && r <= maxR && 0 <= c && c <= maxC

antinodes1 :: (Int, Int) -> Antennas -> Antinodes
antinodes1 dims = Map.foldl' step mempty
  where
    step acc as =
      Set.unions
      $ acc
      : [ Set.singleton p
        | [p1, p2] <- fmap (take 2) <$> List.permutations $ Set.toList as
        , let p = p2 `posAdd` (p2 `posDiff` p1)
        , posIsOnMap dims p
        ]

antinodes2 :: (Int, Int) -> Antennas -> Antinodes
antinodes2 dims = Map.foldl' step mempty
  where
    step acc = Set.unions . (acc :) . genAllAntinodes

    -- Generate all antinode positions from set of antenna positions.
    genAllAntinodes :: Set Pos -> [Set Pos]
    genAllAntinodes as = do
        [p1, p2] <- fmap (take 2) <$> List.permutations $ Set.toList as
        -- p1 gets included by other permutation(s)
        let ps = p2 : genAntinodes p1 p2 []
        pure $ Set.fromList ps

    -- Generate antinode positions from two antenna positions,
    -- stopping when generated positions are outside of the map.
    genAntinodes :: Pos -> Pos -> [Pos] -> [Pos]
    genAntinodes p1 p2 acc =
      let p = p2 `posAdd` (p2 `posDiff` p1)
      in if posIsOnMap dims p then genAntinodes p2 p (p : acc) else acc

solve :: Map Pos Char -> IO ()
solve mp = do
  let dims = getDims mp
      as = mkAntennas mp
      antis1 = antinodes1 dims as
      antis2 = antinodes2 dims as
  putStrLn $ "Part 1: " ++ show (Set.size antis1) ++ " unique antinodes"
  putStrLn $ "Part 1: " ++ show (Set.size antis2) ++ " unique antinodes with resonants"
