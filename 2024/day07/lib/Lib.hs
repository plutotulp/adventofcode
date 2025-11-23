{-# language DerivingStrategies #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe qualified as Maybe
import Control.Parallel.Strategies ( parMap, rseq )
import Data.Functor (void)
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec ( MonadParsec, Parsec, ParseErrorBundle, Token )
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Data.Text.Text

lexeme :: (MonadParsec e s m, Token s ~ Char)=> m a -> m a
lexeme = Lexer.lexeme Char.hspace

decimal :: Parser Int
decimal = lexeme Lexer.decimal

pChar :: Char -> Parser Char
pChar = lexeme . Char.char

type Equation = NonEmpty Int

mkEquation :: Int -> [Int] -> Equation
mkEquation = (:|)

parseEquation :: Parser Equation
parseEquation =
  mkEquation
  <$> (decimal <* pChar ':')
  <*> Megaparsec.many decimal
  <* Megaparsec.optional Char.newline

parser :: Parser [Equation]
parser = Megaparsec.many parseEquation

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) [NonEmpty Int]
parse = Megaparsec.runParser parser ""

hasTwoOpSolution :: Equation -> Bool
hasTwoOpSolution (t :| es) = not $ null (go 0 es)
  where
    go :: Int -> [Int] -> [()]
    go a (x : xs) = do
      guard (a <= t)
      go (a + x) xs <|> go (a * x) xs

    go a [] = void (guard (a == t))

hasThreeOpSolution :: Equation -> Bool
hasThreeOpSolution (t :| es) = not $ null (go 0 es)
  where
    go :: Int -> [Int] -> [()]
    go a (x : xs) = do
      guard (a <= t)
      go (a + x) xs <|> go (a * x) xs <|> go (opcat a x) xs

    go a [] = void (guard (a == t))

    opcat a b = read (show a ++ show b)

parCalibration :: [Equation] -> (Equation -> Bool) -> Int
parCalibration es p = sum $ Maybe.catMaybes $ parMap rseq f es
  where
    f e@(t :| _) = if p e then Just t else Nothing

solve :: [Equation] -> IO ()
solve es = do
  putStrLn $
    "Part 1: 2-op calibration result is "
    ++ show (parCalibration es hasTwoOpSolution)
  putStrLn $
    "Part 2: 3-op calibration result is "
    ++ show (parCalibration es hasThreeOpSolution)
