{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Bool qualified as Bool
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
lexeme = Lexer.lexeme Char.hspace

decimal :: Parser Int
decimal = lexeme Lexer.decimal

char :: Char -> Parser Char
char = lexeme . Char.char

type Id = Int

type Range = (Id, Id)

type Available = Id

-- Database contains ranges of ingredients that are fresh, and list of
-- all available ingredients.
type Db = ([Range], [Available])

parser :: Parser Db
parser =
  (,)
    <$> ( Megaparsec.many
            ( (,)
                <$> (decimal <* char '-')
                <*> (decimal <* Megaparsec.optional Char.newline)
            )
            <* Char.newline
        )
    <*> Megaparsec.many (decimal <* Megaparsec.optional Char.newline)

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) Db
parse = Megaparsec.runParser parser ""

inRange :: Int -> Range -> Bool
inRange val (from, to) = from <= val && val <= to

rangeSize :: Range -> Int
rangeSize (from, to) = 1 + to - from

isFresh :: [Range] -> Id -> Bool
isFresh fss i = any (i `inRange`) fss

indicate :: (a -> Bool) -> a -> Int
indicate p = Bool.bool 0 1 . p

mergeRanges :: [Range] -> [Range]
mergeRanges = merge . List.sort
  where
    merge (r1@(from1, to1) : r2@(from2, to2) : rs)
      | from2 `inRange` r1 || to1 + 1 == from2 = let r3 = (from1, max to1 to2) in merge (r3 : rs)
      | otherwise = r1 : merge (r2 : rs)
    merge [r] = [r]
    merge [] = []

countFreshIds :: [Range] -> Int
countFreshIds = sum . fmap rangeSize . mergeRanges

solve :: Db -> IO ()
solve (frs, ais) = do
  putStrLn $
    "Part 1: "
      ++ show (sum $ indicate (isFresh frs) <$> ais)
      ++ " available ingredients are fresh"
  putStrLn $
    "Part 2: "
      ++ show (countFreshIds frs)
      ++ " ingredient IDs are considered fresh"

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . Megaparsec.errorBundlePretty) solve $ parse input
