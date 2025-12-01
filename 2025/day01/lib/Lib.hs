{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative (Alternative ((<|>)))
import Data.List qualified as List
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Parsec, Token)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Data.Text.Text

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = Lexer.lexeme Char.space

decimal :: Parser Int
decimal = lexeme Lexer.decimal

char :: Char -> Parser Char
char = lexeme . Char.char

data Dir = L | R
  deriving (Eq, Show)

type Rot = (Dir, Int)

toInt :: Rot -> Int
toInt = \case
  (L, v) -> negate v
  (R, v) -> v

parser :: Parser [Rot]
parser =
  Megaparsec.many $
    (,)
      <$> ((L <$ char 'L') <|> (R <$ char 'R'))
      <*> decimal

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) [Rot]
parse = Megaparsec.runParser parser ""

apply1 :: Int -> Rot -> Int
apply1 dial rot = (dial + toInt rot) `mod` 100

password1 :: Int -> [Rot] -> Int
password1 start = length . filter (== 0) . List.scanl apply1 start

apply2 :: Int -> Rot -> (Int, Int)
apply2 dial rot =
  case rot of
    (R, amt) -> turnRight amt
    (L, amt) -> turnLeft amt
  where
    turnRight amt =
      (dial + amt) `divMod` 100
    turnLeft amt =
      -- treat turning left as turning right on a reversed dial, then
      -- reverse the result
      let (n, res) = ((100 - dial) `mod` 100 + amt) `divMod` 100
      in (n, (100 - res) `mod` 100)

password2 :: Int -> [Rot] -> Int
password2 start =
  sum
  . fmap fst
  . List.scanl (apply2 . snd) (0, start)

solve :: [Rot] -> IO ()
solve rs = do
  let dialStart = 50
  putStrLn $ "Part 1: Password is " ++ show (password1 dialStart rs)
  putStrLn $ "Part 2: Password is *really* " ++ show (password2 dialStart rs)
