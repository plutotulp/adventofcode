{-# language DerivingStrategies #-}

module Lib where

import Control.Applicative ( Alternative((<|>)), empty )
import Control.Monad ( (>=>), guard )
import Control.Monad.Logic ( Logic )
import Control.Monad.Logic qualified as Logic
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

parser :: Parser ([Rule], [Update])
parser = do
  rules <- Megaparsec.many (parseRule <* Char.newline)
  _ <- Char.newline
  updates <- Megaparsec.many (parseUpdate <* Char.newline)
  pure (rules, updates)

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

choose :: [a] -> Logic a
choose = foldr ((<|>) . pure) empty

before :: [Rule] -> Int -> Logic Int
before rs p = do
  a `Before` b <- choose rs
  guard (b == p)
  pure a

correctlyOrdered :: [Rule] -> Update -> Logic ()
correctlyOrdered rs = go
  where
    go (p1 : p2 : ps) = do
      bp2 <- before rs p2
      guard (bp2 == p1)
      go (p2 : ps)
    go _ = pure ()

witnessed :: Logic a -> Bool
witnessed = not . null . Logic.observeAll

isCorrectlyOrdered :: [Rule] -> Update -> Bool
isCorrectlyOrdered rs = witnessed . correctlyOrdered rs

update :: [Rule] -> IntSet -> Logic Update
update _  ps  | IntSet.size ps <= 1 = pure (IntSet.toList ps)
update rs ps0 = go (IntSet.toList ps0) []
  where
    go [] = pure
    go (p : ps) = ins p >=> go ps

    ins x []         = pure [x]
    ins x yss@(y:ys) = insBefore <|> insLater
      where
        insBefore = do
          a `Before` b <- choose rs
          guard $ a == x
          guard $ y == b
          pure $ x : yss
        insLater = (y :) <$> ins x ys

-- assumes a correct order exists
reorder :: [Rule] -> Update -> Update
reorder rs = Logic.observe . update rs . IntSet.fromList

-- assumes non-zero and odd-numbered list
getMiddle :: [a] -> a
getMiddle xs = xs !! (length xs `div` 2)

solve :: ([Rule], [Update]) -> IO ()
solve (rs, us) = do
  let (ordered, disordered) = List.partition (isCorrectlyOrdered rs) us
      reordered = reorder rs <$> disordered
      scm = sum $ getMiddle <$> ordered
      scedm = sum $ getMiddle <$> reordered
  putStrLn $ "Part 1: Sum of middles of correctly-ordered updates is " ++ show scm
  putStrLn $ "Part 2: Sum of moddles of corrected updates is " ++ show scedm
