{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}

import Control.Monad.Combinators qualified as Comb
import Data.FileEmbed qualified as FileEmbed
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, Token, Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

inputFile :: Text
inputFile = $(FileEmbed.embedStringFile "input")

lexeme :: (MonadParsec e s m, Token s ~ Char)=> m a -> m a
lexeme = Lexer.lexeme Char.hspace

decimal :: Parser Int
decimal = lexeme Lexer.decimal

type Report = [Int]

parser :: Parser [Report]
parser = Comb.many (Comb.some decimal <* Char.newline)

isSafe :: Report -> Bool
isSafe ls = (isIncreasing || isDecreasing) && hasSmallSteps
  where
    diffs = zipWith (-) (drop 1 ls) ls
    isIncreasing = all (> 0) diffs
    isDecreasing = all (< 0) diffs
    isSmallStep step = 1 <= step && step <= 3
    hasSmallSteps = all (isSmallStep . abs) diffs

data ReportZipper =
  MkRZ
  { dmpCap :: Int        -- ^ Problem Dampener capacity
  , lhs    :: [Int]      -- ^ Entries to the left of focus, in reverse order
  , focus  :: (Int, Int) -- ^ Apply predicate here
  , rhs    :: [Int]      -- ^ Entries to the right of focus
  }
  deriving (Show)

-- Attach zipper to left side of list, if list is long enough to fill
-- focus.
mkReportZipper :: Int ->  [Int] -> Maybe ReportZipper
mkReportZipper dmpCap' xs = case xs of
  a : b : rs -> Just $ MkRZ dmpCap' [] (a,b) rs
  _          -> Nothing

runReportZipper :: (Int -> Int -> Bool) -> ReportZipper -> Bool
runReportZipper predicate = go
  where
    go zp
      | uncurry predicate (focus zp) = moveRight  zp
      | otherwise = discardOne zp

    moveRight zp = case rhs zp of
        r : rs ->
          let (a, b) = focus zp
          in go zp { lhs = a : lhs zp, focus = (b, r), rhs = rs }
        [] -> True

    discardOne zp = case dmpCap zp of
        0 -> False
        _ -> let zp' = zp { dmpCap = dmpCap zp - 1 }
             in discardLeft zp' || discardRight zp'

    discardLeft zp = case lhs zp of
      l : ls ->
        let (_, b) = focus zp
        in go zp { lhs = ls, focus = (l, b) }
      [] -> False

    discardRight zp = case rhs zp of
      r : rs ->
        let (a, _) = focus zp
        in go zp { rhs = rs, focus = (a, r) }
      [] -> False

dampenedIsSafe :: Report -> Bool
dampenedIsSafe = maybe True runZipper . mkReportZipper 1
  where
    runZipper zp = runReportZipper pInc zp  || runReportZipper pDec zp
    pInc x y = let d = y - x in  1 <= d && d <=  3
    pDec x y = let d = y - x in -3 <= d && d <= -1

solve :: [Report] -> IO ()
solve rs = do
  let nSafe    = length $ filter id $ isSafe <$> rs
      nDmpSafe = length $ filter id $ dampenedIsSafe <$> rs
  putStrLn $ "Part 1: " ++ show nSafe    ++ " reports are safe"
  putStrLn $ "Part 2: " ++ show nDmpSafe ++ " reports are safe with dampener"

main :: IO ()
main =
  either (error . show) solve $ Megaparsec.runParser parser "" inputFile
