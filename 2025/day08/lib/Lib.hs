module Lib where

-- import Data.List qualified as List

import Control.Monad.State ( State )
import Control.Monad.State qualified as State
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Set ( Set )
import Data.Set qualified as Set
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

type Pos = (Int, Int, Int)

parser :: Parser [Pos]
parser =
  Megaparsec.many $
    (,,)
      <$> (decimal <* char ',')
      <*> (decimal <* char ',')
      <*> decimal

parse :: Data.Text.Text -> Either (ParseErrorBundle Data.Text.Text Void) [Pos]
parse = Megaparsec.runParser parser ""

distSq :: Pos -> Pos -> Int
distSq (x1, y1, z1) (x2, y2, z2) = dx * dx + dy * dy + dz * dz
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1

allDistSqs :: [Pos] -> [(Int, (Pos, Pos))]
allDistSqs = concatMap go . List.tails
  where
    go (p : ps) = ann p <$> ps
    go [] = []

    ann p1 p2 = (distSq p1 p2, (p1, p2))

connect :: (Pos, Pos) -> State [Set Pos] ()
connect (p1, p2) = do
  mc1 <- getCircuit p1
  mc2 <- getCircuit p2
  State.modify' $
    case (mc1, mc2) of
      (Just c1, Just c2) -> (Set.unions [c1, c2] :)
      (Just c1, Nothing) -> (Set.insert p2 c1 :)
      (Nothing, Just c2) -> (Set.insert p1 c2 :)
      (Nothing, Nothing) -> (Set.fromList [p1, p2] :)

getCircuit :: Pos -> State [Set Pos] (Maybe (Set Pos))
getCircuit p = do
  (cs, csRest) <- State.gets $ List.partition (Set.member p)
  case cs of
    [c] -> State.put csRest >> pure (Just c)
    [] -> pure Nothing
    _ -> error $ "found junction in multiple circuits " ++ show cs

connectNearest :: Int -> [Pos] -> [Set Pos]
connectNearest n =
  flip State.execState [] . go
  where
    go =
      Foldable.traverse_ connect . take n . fmap snd . List.sort . allDistSqs

solve :: [Pos] -> IO ()
solve ps = do
  let
    n = 1000
    cs = connectNearest n ps
  putStrLn $
    "Part 1: Product of size of the three largerst circuits after "
    ++ show n ++ " connections is "
    ++ show (product $ take 3 $ List.sortBy (flip compare) $ Set.size <$> cs)
  putStrLn $
    "Part 2: "

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . Megaparsec.errorBundlePretty) solve $ parse input
