module Lib where

import Control.Applicative ((<|>))
import Data.Bifunctor qualified as Bifunctor
import Data.List ((!?))
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.IO.Utf8 qualified as IO.Utf8
import Data.Void (Void)
import System.Environment qualified as Environment
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Parsec, Token)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Debug.Trace

type Parser = Parsec Void Data.Text.Text

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = Lexer.lexeme Char.hspace

decimal :: Parser Int
decimal = lexeme Lexer.decimal

char :: Char -> Parser Char
char = lexeme . Char.char

data Op = Add | Mul
  deriving (Eq, Show)

newtype Off = Off Int
  deriving (Eq, Ord, Show)

newtype Col = Col Int
  deriving (Eq, Ord, Show)

toOff :: Col -> Col -> Off
toOff (Col col0) (Col col) = Off $ col0 - col

data Entry = EInt Int Col | EOp Op
  deriving (Eq, Show)

type Db = [[Entry]]

parser :: Parser Db
parser =
  Megaparsec.sepBy
    (Megaparsec.many (eInt <|> eOp))
    (Char.newline *> Char.space)
    <* Megaparsec.eof
  where
    add = Add <$ char '+'
    mul = Mul <$ char '*'
    eOp = EOp <$> (add <|> mul)
    eInt = EInt <$> decimal <*> getCol
    getCol =
      Col . Megaparsec.unPos . Megaparsec.sourceColumn
        <$> Megaparsec.getSourcePos

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) Db
parse = Megaparsec.runParser parser ""

type Eqn = (Op, [Int])

humanConvertInts :: [(Int, Off)] -> [Int]
humanConvertInts = fmap fst

-- debug
showVs :: [(Int, Off)] -> String
showVs vs = unlines $ go <$> reverse vs
  where
    numLen (i, Off o) = length (show i) + o
    num = maximum (numLen <$> vs)
    go (i, Off o) =
      let
        n = length $ show i
        s = num - (n + o)
      in
        "  @" ++ show o ++ "|" ++ replicate s ' ' ++ show i

-- bleh, what the hell
cephalopodConvertInts :: [(Int, Off)] -> [Int]
cephalopodConvertInts vs0 = let res = go vs0 <$> num in trace ("convert into " ++ show res ++ " from\n" ++ showVs vs0) res
  where
    numLen (i, Off o) = length (show i) + o
    num = [1 .. maximum (numLen <$> vs0)]

    go vs n =
      let step acc (v, Off o) =
            let str = show v
             in maybe
                  acc
                  (((acc * 10) +) . read . pure)
                  (str !? ((length str + o) - n))
       in foldl' step 0 (reverse vs)

toEqns :: ([(Int, Off)] -> [Int]) -> Db -> [Eqn]
toEqns toInts = fmap (convert . reverse) . List.transpose
  where
    convert :: [Entry] -> Eqn
    convert (EOp o : vs) =
      let ps = toPairs <$> vs
          c0 = maximum $ snd <$> ps
          ps' = Bifunctor.second (toOff c0) <$> ps
       in (o, toInts ps')
    convert e = error $ "expected op followed by ints, but got " ++ show e

    toPairs (EInt i o) = (i, o)
    toPairs e = error $ "expected int with offset, but got " ++ show e

-- Assumes at least one value in eqn.
calc :: Eqn -> Int
calc (Add, vs) = sum vs
calc (Mul, vs) = product vs

grandTotal :: [Eqn] -> Int
grandTotal = sum . fmap calc

solve :: Db -> IO ()
solve db = do
  putStrLn $
    "Part 1: human grand total is "
      ++ show (grandTotal $ toEqns humanConvertInts db)
  putStrLn $
    "Part 2: cephalopod grand total is "
      ++ show (grandTotal $ toEqns cephalopodConvertInts db)

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . Megaparsec.errorBundlePretty) solve $ parse input
