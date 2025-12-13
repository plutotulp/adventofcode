module Lib where

-- import Data.List qualified as List

import Control.Monad.State ( State )
import Control.Monad.State qualified as State
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
import Data.Bifunctor qualified as Bifunctor

type Parser = Parsec Void Data.Text.Text

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = Lexer.lexeme Char.space

decimal :: Parser Int
decimal = lexeme Lexer.decimal

char :: Char -> Parser Char
char = lexeme . Char.char

type Pos = (Int, Int, Int)

posX :: Pos -> Int
posX (x, _, _) = x

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

type Circuit = Set Pos

type Connection = (Pos, Pos)

connect :: Connection -> [Circuit] -> [Circuit]
connect (p1, p2) = State.execState go
  where
    go = do
      mc1 <- extractCircuit p1
      mc2 <- extractCircuit p2
      State.modify' $
        case (mc1, mc2) of
          (Just c1, Just c2) -> (Set.unions [c1, c2] :)
          (Just c1, Nothing) -> (Set.insert p2 c1 :)
          (Nothing, Just c2) -> (Set.insert p1 c2 :)
          (Nothing, Nothing) -> (Set.fromList [p1, p2] :)

-- Lookup circuit containing junction at the given position and
-- extract it from the state.
extractCircuit :: Pos -> State [Circuit] (Maybe Circuit)
extractCircuit p = do
  (cs, csRest) <- State.gets $ List.partition (Set.member p)
  case cs of
    [c] -> State.put csRest >> pure (Just c)
    [] -> pure Nothing
    _ -> error $ "found junction in multiple circuits " ++ show cs

-- Yield the stream of curcuits formed by connection all the junctions
-- together one connection at the time. Each entry in the resulting
-- state stream is the list of circuits and the latest connection
-- made. Each new connection made is connection is always between the
-- two junctions without direct connection that are nearest each
-- other.
connectionStream :: [Pos] -> [([Circuit], Maybe Connection)]
connectionStream js0 =
  scanl connect' initCircuits connectionsToAdd
  where
    -- Initially, all junctions make up their own little circuit,
    -- alone.
    initCircuits = (Set.singleton <$> js0, Nothing)

    -- The list of connections to make between junctions, ordered by
    -- closeness between the junctions.
    connectionsToAdd = fmap snd $ List.sort $ allDistSqs js0

    connect' (cs, _) con = (connect con cs, Just con)

connectNearest :: Int -> [Pos] -> [Circuit]
connectNearest n = extract . fmap fst . drop n . connectionStream
  where
    extract [] = error $ "could not make " ++ show n ++ " connections"
    extract (cs : _) = cs

connectUntilOneCircuit :: [Pos] -> Connection
connectUntilOneCircuit = extract . connectionStream
  where
    -- Extract looks for the part of the stream of changes to find the
    -- point where the circuit list went down to a single circuit, and
    -- yields the connection made at that point.
    extract [] = error "connections never formed a single circuit"
    extract ((_, Nothing) : stream) = extract stream
    extract ((cs, Just conn) : stream) =
      case cs of
        [_] -> conn
        _ -> extract stream

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
    "Part 2: Product of X coordinates of the last two junction boxes is "
    ++ show (uncurry (*) $ Bifunctor.bimap posX posX $ connectUntilOneCircuit ps)

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . Megaparsec.errorBundlePretty) solve $ parse input
