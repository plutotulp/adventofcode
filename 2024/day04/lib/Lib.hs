module Lib where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type Col = Int
type Row = Int
type Pos = (Row, Col)

parse :: String -> Map Pos Char
parse str = Map.fromList entries
  where
    ls = lines str
    entries = concat $ zipWith mkRow [0..] ls
    mkRow ir = zipWith (mkCol ir) [0..]
    mkCol ir ic v = ((ir, ic), v)

posAdd :: Pos -> Pos -> Pos
posAdd (ir1, ic1) (ir2, ic2) = (ir1 + ir2, ic1 + ic2)

posNegate :: Pos -> Pos
posNegate (ir, ic) = (negate ir, negate ic)

type Kernel = [(Pos, Char)]

kernE :: String -> Kernel
kernE  = zipWith (\i v -> ((0, i), v)) [0..]

kernW :: String -> Kernel
kernW  = fmap (first posNegate) . kernE

kernS :: String -> Kernel
kernS  = zipWith (\i v -> ((i, 0), v)) [0..]

kernN :: String -> Kernel
kernN  = fmap (first posNegate) . kernS

kElmAdd :: (Pos, Char) -> (Pos, Char) -> (Pos, Char)
kElmAdd (p1, v1) (p2, _) = (posAdd p1 p2, v1)

kElmOffset :: Pos -> (Pos, Char) -> (Pos, Char)
kElmOffset off (p, v) = (posAdd off p, v)

kernSE :: String -> Kernel
kernSE mask = zipWith kElmAdd (kernS mask) (kernE mask)

kernSW :: String -> Kernel
kernSW mask = zipWith kElmAdd (kernS mask) (kernW mask)

kernNW :: String -> Kernel
kernNW mask = zipWith kElmAdd (kernN mask) (kernW mask)

kernNE :: String -> Kernel
kernNE mask = zipWith kElmAdd (kernN mask) (kernE mask)

matchKernel :: Map Pos Char -> Pos -> Kernel -> Bool
matchKernel mp off = all (match . kElmOffset off)
  where
    match (pos, val) = Map.lookup pos mp == Just val

countMatches :: Map Pos Char -> Pos -> [Kernel] -> Int
countMatches mp off = length . filter id . fmap (matchKernel mp off)

xmasKernels :: String -> [Kernel]
xmasKernels mask =
  [ kernE  mask
  , kernW  mask
  , kernS  mask
  , kernN  mask
  , kernSE mask
  , kernSW mask
  , kernNW mask
  , kernNE mask
  ]

crossMasKernels :: String -> [Kernel]
crossMasKernels mask
  | even (length mask) = error "Mask must have odd length"
  | otherwise =
      let noff = length mask `div` 2
          compact = Set.toList . Set.fromList
          kernSE' = kElmOffset (-noff, -noff) <$> kernSE mask
          kernSW' = kElmOffset (-noff,  noff) <$> kernSW mask
          kernNW' = kElmOffset ( noff,  noff) <$> kernNW mask
          kernNE' = kElmOffset ( noff, -noff) <$> kernNE mask
      in [ compact $ kernSE' ++ kernSW'
         , compact $ kernSE' ++ kernNE'
         , compact $ kernNW' ++ kernNE'
         , compact $ kernNW' ++ kernSW'
         ]

countAllMatches :: Map Pos Char -> [Kernel] -> Int
countAllMatches mp ks = sum $ flip (countMatches mp) ks <$> Map.keys mp

solve :: Map Pos Char -> IO ()
solve mp = do
  let
    x     = xmasKernels "XMAS"
    cross = crossMasKernels "MAS"
  putStrLn $ "Part 1: XMAS appears " ++ show (countAllMatches mp x) ++ " times"
  putStrLn $ "Part 2: MAS appears crossed " ++ show (countAllMatches mp cross) ++ " times"
