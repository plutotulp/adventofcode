{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}

module Lib where

import Control.Applicative ( Alternative((<|>)) )
import Control.Monad ( guard, void )
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Bool (bool)
import Data.Functor ( ($>) )
import Data.Maybe qualified as Maybe
import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec ( MonadParsec, Token, Parsec, ParseErrorBundle, (<?>) )
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Data.Text.Text

lexeme :: (MonadParsec e s m, Token s ~ Char)=> m a -> m a
lexeme = Lexer.lexeme Char.hspace

decimal :: Parser Int
decimal = Lexer.decimal

data Instr
  = Mul Int Int
  | Do
  | Dont
  deriving stock (Eq, Show)

parser :: Parser [Instr]
parser = Maybe.catMaybes <$> Megaparsec.many ((Just <$> Megaparsec.try parseInstr) <|> (eatToken >> pure Nothing))
  where
    eatToken = void (Megaparsec.anySingle <?> "any single token")


parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) [Instr]
parse = Megaparsec.runParser parser ""

parseInstr :: Parser Instr
parseInstr = Megaparsec.try parseMul <|> Megaparsec.try parseDont <|> parseDo

parseDont :: Parser Instr
parseDont = Char.string "don't()" $> Dont

parseDo :: Parser Instr
parseDo = Char.string "do()" $> Do

parseMul :: Parser Instr
parseMul = inner
  where
    isThreeDigits x = 0 <= x && x <= 999
    inner = do
      _ <- Char.string "mul("
      x <- decimal
      _ <- Char.char ','
      guard (isThreeDigits x)
      y <- decimal
      guard (isThreeDigits y)
      _ <- Char.char ')'
      pure $ Mul x y

isMul :: Instr -> Bool
isMul (Mul _ _) = True
isMul _         = False

executeInstr :: Instr -> State Bool (Maybe Int)
executeInstr (Mul a b) = State.gets (bool Nothing (Just (a * b)))
executeInstr Do        = State.put True  $> Nothing
executeInstr Dont      = State.put False $> Nothing

execute :: [Instr] -> [Int]
execute =
  Maybe.catMaybes <$> flip State.evalState True . traverse executeInstr

sumOfMuls :: [Instr] -> Int
sumOfMuls = sum . execute . filter isMul

sumOfEnabledMuls :: [Instr] -> Int
sumOfEnabledMuls = sum . execute

solve :: [Instr]-> IO ()
solve is = do
  putStrLn $ "Part 1: sum of muls is " ++ show (sumOfMuls is)
  putStrLn $ "Part 2: sum of enabled muls is " ++ show (sumOfEnabledMuls is)
