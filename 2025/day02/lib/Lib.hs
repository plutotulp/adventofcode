module Lib where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text.IO.Utf8 qualified as IO.Utf8
import Data.Void (Void)
import System.Environment qualified as Environment
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

type Id = Int

type Range = (Id, Id) -- (first, last)

parser :: Parser [Range]
parser = Megaparsec.sepBy range (char ',')
  where
    range =
      (,)
        <$> (decimal <* char '-')
        <*> decimal

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) [Range]
parse = Megaparsec.runParser parser ""

silly1 :: Int -> Bool
silly1 = check . show
  where
    check s =
      let n = length s
          (p1, p2) = splitAt (n `div` 2) s
       in 1 < n && even n && p1 == p2

silly2 :: Int -> Bool
silly2 = go . show
  where
    go s =
      let n = length s
       in any (check s n) $ drop 1 $ List.inits $ take (n `div` 2) s
    check s n p =
      n `rem` length p == 0 && s == take n (cycle p)

invalidsBy :: (Int -> Bool) -> Range -> [Int]
invalidsBy p = filter p . uncurry enumFromTo

solve :: [Range] -> IO ()
solve rs = do
  putStrLn $
    "Part 1: The sum of all invalid IDs is "
      ++ show (sum $ concatMap (invalidsBy silly1) rs)
  putStrLn $
    "Part 2: The sum of all invalid IDs using new rules is "
      ++ show (sum $ concatMap (invalidsBy silly2) rs)

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . show) solve $ parse input
