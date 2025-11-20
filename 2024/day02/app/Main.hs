import Data.List qualified as List
import Data.Text (Text)
import Data.Text.IO.Utf8 qualified as IO.Utf8
import Data.Void (Void)
import System.Environment qualified as Environment
import Text.Megaparsec (MonadParsec, Token, Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

lexeme :: (MonadParsec e s m, Token s ~ Char)=> m a -> m a
lexeme = Lexer.lexeme Char.hspace

decimal :: Parser Int
decimal = lexeme Lexer.decimal

type Report = [Int]

parser :: Parser [Report]
parser = Megaparsec.many (Megaparsec.some decimal <* Char.newline)

isSafe :: Report -> Bool
isSafe ls = (isIncreasing || isDecreasing) && hasSmallSteps
  where
    diffs = zipWith (-) (drop 1 ls) ls
    isIncreasing = all (> 0) diffs
    isDecreasing = all (< 0) diffs
    isSmallStep step = 1 <= step && step <= 3
    hasSmallSteps = all (isSmallStep . abs) diffs

dampenedIsSafe :: Report -> Bool
dampenedIsSafe = any isSafe . reports
  where
    reports r = r : allDampened r
    allDampened ls =
      zipWith (\h t -> h ++ drop 1 t) (List.inits ls) (List.tails ls)

solve :: [Report] -> IO ()
solve rs = do
  let nSafe    = length $ filter id $ isSafe <$> rs
      nDmpSafe = length $ filter id $ dampenedIsSafe <$> rs
  putStrLn $ "Part 1: " ++ show nSafe    ++ " reports are safe"
  putStrLn $ "Part 2: " ++ show nDmpSafe ++ " reports are safe with dampener"

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . show) solve $ Megaparsec.runParser parser "" input
