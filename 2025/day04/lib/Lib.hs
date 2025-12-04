module Lib (main) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import System.Environment qualified as Environment

type Col = Int

type Row = Int

newtype Pos = Pos {unPos :: (Row, Col)}
  deriving (Eq, Ord, Show)

parse :: String -> Map Pos Char
parse str = Map.fromList entries
  where
    ls = lines str
    entries = concat $ zipWith mkRow [0 ..] ls
    mkRow ir = zipWith (mkCol ir) [0 ..]
    mkCol ir ic v = (Pos (ir, ic), v)

newtype Off = Off {unOff :: (Row, Col)}
  deriving (Eq, Ord, Show)

posAdd :: Pos -> Off -> Pos
posAdd (Pos (r1, c1)) (Off (r2, c2)) = Pos (r1 + r2, c1 + c2)

adjacent :: [Off]
adjacent =
  [ Off (-1, -1),
    Off (-1, 0),
    Off (-1, 1),
    Off (0, 1),
    Off (1, 1),
    Off (1, 0),
    Off (1, -1),
    Off (0, -1)
  ]

adjacentPos :: Pos -> [Pos]
adjacentPos p = posAdd p <$> adjacent

isPaperRoll :: Char -> Bool
isPaperRoll = (== '@')

-- Positions of paper rolls that are accessible by fork lift.
forkLiftAccessible :: Map Pos Char -> [Pos]
forkLiftAccessible mp = filter isAccessible paperRolls
  where
    paperRolls = Map.keys $ Map.filter isPaperRoll mp
    isAccessible p = countAdjacent p < 4
    countAdjacent =
      length
        . filter isPaperRoll
        . Maybe.mapMaybe (`Map.lookup` mp)
        . adjacentPos

-- Positions of paper rolls that can be removed, in iterations.
canBeRemoved :: Map Pos Char -> [[Pos]]
canBeRemoved = go
  where
    go mp =
      let ps  = forkLiftAccessible mp
          mp' = foldl' (flip Map.delete) mp ps
      in if null ps then [] else ps : go mp'

solve :: Map Pos Char -> IO ()
solve mp = do
  putStrLn $
    "Part 1: There are "
      ++ show (length $ forkLiftAccessible mp)
      ++ " paper rolls accessible by forklift"
  putStrLn
    $ "Part 2: We can remove "
    ++ show (length $ concat $ canBeRemoved mp)
    ++ " paper rolls in total"

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- readFile inputFile
  solve $ parse input
