{-# language DerivingStrategies #-}

module Lib where

import Control.Applicative ( Alternative((<|>)) )
import Control.Monad ( (>=>), guard )
import Data.IntMap.Strict ( IntMap )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet ( IntSet )
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec ( Parsec, ParseErrorBundle)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Data.Text.Text

type Page = Int

data Rule = Page `Before` Page
  deriving stock (Eq, Show)

type Update = [Int]

-- maps page to pages that are before it
type Beforemap = IntMap IntSet

parser :: Parser ([Rule], [Update])
parser = do
  rs <- Megaparsec.many (parseRule <* Char.newline)
  _ <- Char.newline
  us <- Megaparsec.many (parseUpdate <* Char.newline)
  pure (rs, us)

parseRule :: Parser Rule
parseRule = do
  a <- Lexer.decimal
  _ <- Char.char '|'
  b <- Lexer.decimal
  pure $ a `Before` b

parseUpdate :: Parser Update
parseUpdate = Megaparsec.sepBy Lexer.decimal (Char.char ',')

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) ([Rule], [Update])
parse = Megaparsec.runParser parser ""

mkBeforemap :: [Rule] -> Beforemap
mkBeforemap = IntMap.fromListWith IntSet.union . fmap toKv
  where
    toKv (a `Before` b) = (b, IntSet.singleton a)

-- all pages that are before the given one, directly by rule
befores :: Int -> Beforemap -> [Int]
befores p = maybe [] IntSet.toList . IntMap.lookup p

isBefore :: Int -> Int -> Beforemap -> Bool
isBefore b x bs = maybe False (IntSet.member b) (IntMap.lookup x bs)

isCorrectlyOrdered :: Beforemap -> Update -> Bool
isCorrectlyOrdered bs = not . null . go
  where
    go (p1 : p2 : ps) = do
      bp2 <- befores p2 bs
      guard (bp2 == p1)
      go (p2 : ps)
    go _ = pure ()

updates :: Beforemap -> IntSet -> [Update]
updates _  ps  | IntSet.size ps <= 1 = pure (IntSet.toList ps)
updates bs ps0 = go (IntSet.toList ps0) []
  where
    go [] = pure
    go (p : ps) = ins p >=> go ps

    ins x []         = pure [x]
    ins x yss@(y:ys) = insBefore <|> insLater
      where
        insBefore
          | isBefore x y bs = pure $ x : yss
          | otherwise       = []
        insLater = (y :) <$> ins x ys

-- assumes a correct order exists, errors if not
reorder :: Beforemap -> Update -> Update
reorder bs u0 =
  case updates bs $ IntSet.fromList u0 of
    u : _ -> u
    []    -> error "no correct order exists"

-- errors on empty list or even number of elements
getMiddle :: Show a => [a] -> a
getMiddle [] =
  error "no middle in empty list"
getMiddle xs =
  let n = length xs
  in if even n
     then error "even number of elements in list"
     else xs !! (n `div` 2)

solve :: ([Rule], [Update]) -> IO ()
solve (rs, us) = do
  let bs = mkBeforemap rs
      (ordered, disordered) = List.partition (isCorrectlyOrdered bs) us
      reordered = reorder bs <$> disordered
  putStrLn $
    "Part 1: Sum of middles of correctly-ordered updates is "
    ++ show (sum $ getMiddle <$> ordered)
  putStrLn $
    "Part 2: Sum of moddles of corrected updates is "
    ++ show (sum $ getMiddle <$> reordered)
