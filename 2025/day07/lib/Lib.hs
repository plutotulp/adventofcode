{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe qualified as Maybe
import System.Environment qualified as Environment

data Item
  = Start
  | Splitter
  deriving (Eq, Show)

newtype Col = Col Int
  deriving (Eq, Ord, Show)

type Diagram = [[(Item, Col)]]

parse :: String -> Diagram
parse = fmap (Maybe.mapMaybe mkItem . zip [0 ..]) . lines
  where
    mkItem = \case
      (c, 'S') -> Just (Start, Col c)
      (c, '^') -> Just (Splitter, Col c)
      _ -> Nothing

-- State is the pair of number of total splits, and map from beam
-- column to number of timelines.
type St = (Int, IntMap Int)

startBeam :: Col -> State St ()
startBeam (Col c) = State.modify' $ Bifunctor.second (IntMap.insert c 1)

maybeSplitBeam :: Col -> State St ()
maybeSplitBeam (Col sc) = State.modify' check
  where
    check st@(_, cs) = maybe st (split st) $ IntMap.lookup sc cs

    split (ns, cs) nb =
      ( ns + 1,
        cs
          & IntMap.delete sc
          & IntMap.insertWith (+) (sc - 1) nb
          & IntMap.insertWith (+) (sc + 1) nb
      )

stepBeam :: [(Item, Col)] -> State St ()
stepBeam = Foldable.traverse_ go
  where
    go (Start, c) = startBeam c
    go (Splitter, c) = maybeSplitBeam c

runDiagram :: Diagram -> St
runDiagram = flip State.execState (0, mempty) . Foldable.traverse_ stepBeam

timelines :: St -> Int
timelines = sum . IntMap.elems . snd

solve :: Diagram -> IO ()
solve dia = do
  let st = runDiagram dia
  putStrLn $
    "Part 1: Beam will be split " ++ show (fst st) ++ " times"
  putStrLn $
    "Part 2: Beam will end up in " ++ show (timelines st) ++ " timelines"

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- readFile inputFile
  Lib.solve $ Lib.parse input
