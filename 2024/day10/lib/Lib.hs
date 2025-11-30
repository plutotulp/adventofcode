module Lib (parse, solve) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8)

type Col = Int

type Row = Int

newtype Pos = Pos {unPos :: (Row, Col)}
  deriving (Eq, Ord, Show)

type TopoMap = Map Pos Word8

parse :: String -> TopoMap
parse str = Map.fromList entries
  where
    ls = lines str
    entries = concat $ zipWith mkRow [0 ..] ls
    mkRow ir = zipWith (mkCol ir) [0 ..]
    mkCol ir ic v = (Pos (ir, ic), read $ pure v)

newtype Dir = Dir {unDir :: (Row, Col)}
  deriving (Eq, Ord, Show)

dirs :: [Dir]
dirs =
  [ Dir (0, -1),
    Dir (0, 1),
    Dir (-1, 0),
    Dir (1, 0)
  ]

dirInv :: Dir -> Dir
dirInv (Dir (r, c)) = Dir (negate r, negate c)

dirsExcept :: Dir -> [Dir]
dirsExcept d = filter (/= d) dirs

move :: Pos -> Dir -> Pos
move (Pos (r1, c1)) (Dir (r2, c2)) = Pos (r1 + r2, c1 + c2)

keysFor :: (Eq v) => v -> Map k v -> [k]
keysFor v =
  fmap fst . filter ((== v) . snd) . Map.toList

trailHeadLocs :: TopoMap -> [Pos]
trailHeadLocs = keysFor 0

trails :: TopoMap -> Word8 -> Pos -> [Dir] -> [[(Pos, Word8)]]
trails mp maxH p0 ds0 = maybe mempty start $ p0 `Map.lookup` mp
  where
    start h0 = go h0 p0 ds0

    -- The go function operates in the list monad, for
    -- no-deterministic computation. Each new trail location produces
    -- zero or more successful next steps. Any branch of steps that
    -- ultimately fails get pruned away.
    go h p _
      | h == maxH =
          -- reached top, yield successfully
          [[(p, h)]]
    go h p ds = do
      d <- ds
      let p' = move p d
          h' = h + 1
          ds' = dirsExcept (dirInv d)
      case Map.lookup p' mp of
        Just v
          | v == h' ->
              -- found next step, keep going by attaching the current
              -- step to all other succesful subsequent trails produced
              -- by moving along the next step.
              ((p, h) :) <$> go h' p' ds'
        _ ->
          -- signal failure, disregarding this branch
          []

score :: TopoMap -> Pos -> Int
score mp p = length $ Set.fromList $ last <$> trails mp 9 p dirs

rating :: TopoMap -> Pos -> Int
rating mp p = length $ trails mp 9 p dirs

solve :: TopoMap -> IO ()
solve mp = do
  let ths = trailHeadLocs mp
  putStrLn $
    "Part 1: Sum of trailhead scores is "
      ++ show (sum $ score mp <$> ths)
  putStrLn $
    "Part 1: Sum of trailhead ratings is "
      ++ show (sum $ rating mp <$> ths)
