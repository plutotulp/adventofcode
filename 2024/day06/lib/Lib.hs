{-# language LambdaCase #-}

module Lib ( parse, solve )where

import Control.Monad.State ( State )
import Control.Monad.State qualified as State
import Control.Parallel.Strategies ( parMap, rseq )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map
import Data.Set ( Set )
import Data.Set qualified as Set

type Col = Int
type Row = Int

newtype Pos = Pos { unPos :: (Row, Col) }
  deriving stock (Eq, Ord, Show)

moveDir :: Pos -> Dir -> Pos
moveDir (Pos (r1, c1)) (Dir (rd, cd)) = Pos (r1 + rd, c1 + cd)

newtype Dir = Dir { unDir :: (Row, Col) }
  deriving stock (Eq, Ord, Show)

-- Guard state is position and an endless list of directions. The
-- guard is currently facing the direction as the head of the list. At
-- the next obstacle, the guard will rotate to face the next direction
-- in the list.
type Guard = (Pos, [Dir])

getGuardPos :: Guard -> Pos
getGuardPos = fst

mkGuard :: Pos -> Dir -> Guard
mkGuard p d = (p, dropWhile (/= d) (cycle dirs))

parse :: String -> Map Pos Char
parse str = Map.fromList entries
  where
    ls = lines str
    entries = concat $ zipWith mkRow [0..] ls
    mkRow ir = zipWith (mkCol ir) [0..]
    mkCol ir ic v = (Pos (ir, ic), v)

-- All tiles in map, broken down to "is this an obstacle?"
type Cfg = Map Pos Bool

-- 1. Guard's memory of visited positions and in which directions they
-- were visited. 2. Guard's position and current + all future
-- directions.
type St = (Set (Pos, Dir), Guard)

getSeen :: St -> Set (Pos, Dir)
getSeen = fst

getGuard :: St -> Guard
getGuard = snd

-- assumed to be [N, E, S, W]
guardChars :: [Char]
guardChars = [ '^', '>', 'v', '<' ]

-- assumed to be [N, E, S, W]
dirs :: [Dir]
dirs = [Dir (-1,  0), Dir ( 0,  1), Dir ( 1,  0), Dir (0, -1)]

guardCharToDir :: Char -> Dir
guardCharToDir ch =
  case filter ((== ch) . fst) $ zip guardChars dirs of
    [x] -> snd x
    [ ] -> error "you sneaky people did not provide a guard char"
    _   -> error "whoever heard of type safety?"

-- assumes there is one guard on the input map
initApp :: Map Pos Char -> (Cfg, St)
initApp mp =
  case maybeGuard of
    Nothing    -> error "guard not found on map"
    Just guard -> (cfg, (mempty, guard))
  where
    (cfg, maybeGuard) = flip State.runState Nothing $ do
      flip Map.traverseWithKey mp $ \pos ch ->
        case ch of
          '.' -> pure False
          '#' -> pure True
          x | x `elem` guardChars -> do
                 let guard = mkGuard pos (guardCharToDir x)
                 State.put (Just guard)
                 pure False
          _ -> error "unexpected map tile"

data Res
  = UpdatedGuard Guard
  | Terminated TermReason
  deriving (Eq, Show)

data TermReason = DetectedLoop| LeftMap
  deriving (Eq, Show)

-- Move guard one step or turn forward, marking map with occupied
-- position. If guard is outside the map or is a loop, yield Nothing.
step :: Cfg -> State St Res
step cfg = do
  st <- State.get
  case st of
    (seen, guard@(guardPos, guardDirections@(guardDir:guardNewDirections))) -> do
      case (guardPos, guardDir) `Set.member` seen of
        True ->
          -- guard has been here before, facing the same way!
          pure $ Terminated DetectedLoop
        False ->
          let
            -- guard has not fallen off mapp, hence has seen its
            -- current location.
            seen' = Set.insert (guardPos, guardDir) seen
            -- imagine moving the guard along current direction and
            -- considering where it ends up
            guardPos' = moveDir guardPos guardDir
          in case Map.lookup guardPos' cfg of
            Just False -> do
              -- no obstruction, move forward
              let guard' = (guardPos', guardDirections)
              State.put (seen', guard')
              pure $ UpdatedGuard guard'
            Just True -> do
              -- obstructed, turn about
              let guard' = (guardPos, guardNewDirections)
              State.put (seen', guard')
              pure $ UpdatedGuard guard'
            Nothing -> do
              -- not at a map location
              State.put (seen', guard)
              pure $ Terminated LeftMap
    _ -> error "won't reach because guard directions list is infinite"

run :: Cfg -> St -> (TermReason, St)
run cfg = State.runState go
  where
    go = step cfg >>= \case
      UpdatedGuard _ -> go
      Terminated r -> pure r

blockedMaps :: [Pos] -> Map Pos Bool -> [Map Pos Bool]
blockedMaps ps cfg = block <$> ps
  where
    block p = Map.insert p True cfg

solve :: Map Pos Char -> IO ()
solve mp = do
  putStrLn $ "Part 1: " ++ show ngp ++ " distinct guard positions"
  putStrLn $ "Part 2: " ++ show nop ++ " valid obstruction placements"
  where
    (cfg, st0) = initApp mp

    -- State after initial run
    st1 = snd $ run cfg st0

    -- All guard locations during the first run (disregarding which
    -- way they were facing).
    seen1 :: Set Pos
    seen1 = Set.map fst $ getSeen st1

    -- Number of guard positions during initial run.
    ngp :: Int
    ngp = Set.size seen1

    -- Initial guard position.
    guardPos0 :: Pos
    guardPos0 = getGuardPos (getGuard st0)

    -- Locations to consider for placing obstruction, which is the
    -- same as those that the guard visited during the initial run.
    -- Other locations are out of reach for the guard. But we remove
    -- the initial guard position from the list, because then the
    -- guard would be starting out inside an obstruction.
    ops :: Set Pos
    ops = Set.delete guardPos0 seen1

    -- Number of obstruction positions that result in a looping guard.
    -- A bit slow, so spark all computations in parallel. The runtime
    -- evaluates them in order as tasks become available. Requires ghc
    -- compilation with -threaded (see day06.cabal)
    nop :: Int
    nop =
      length
      $ filter (== DetectedLoop)
      $ parMap rseq (fst . (`run` st0))
      $ blockedMaps (Set.toList ops) cfg
