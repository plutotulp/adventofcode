{-# language TemplateHaskell #-}

import Data.Bifunctor

import Control.Monad.Combinators qualified as Comb
import Data.FileEmbed qualified as FileEmbed
import Data.List qualified as List
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, Token, Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.IntMap qualified as IntMap
import Control.Monad.State qualified as State

type Parser = Parsec Void Text

inputFile :: Text
inputFile = $(FileEmbed.embedStringFile "input")

lexeme :: (MonadParsec e s m, Token s ~ Char)=> m a -> m a
lexeme = Lexer.lexeme Char.space

decimal :: Parser Int
decimal = lexeme Lexer.decimal

parser :: Parser ([Int], [Int])
parser = unzip <$> Comb.many ((,) <$> decimal <*> decimal)

sumDist :: ([Int], [Int]) -> Int
sumDist (list1, list2) = sum $ zipWith (\a b -> abs (a - b)) list1 list2

similarity :: ([Int], [Int]) -> Int
similarity (list1, list2) =
  sum $ State.evalState (traverse go list1) IntMap.empty
  where
    go val =
        maybe (cacheMiss val) pure . IntMap.lookup val =<< State.get

    cacheMiss val = do
      let count = length $ filter (== val) list2
          score = val * count
      State.modify' (IntMap.insert val score)
      pure score

solve :: ([Int], [Int]) -> IO ()
solve lists = do
  let orderedLists = bimap List.sort List.sort lists
  putStrLn $ "Part 1: Total distance is " ++ show (sumDist orderedLists)
  putStrLn $ "Part 2: Total similarity is " ++ show (similarity orderedLists)

main :: IO ()
main =
  either (error . show) solve $ Megaparsec.runParser parser "" inputFile
