import Data.Text.IO.Utf8 qualified as IO.Utf8
import System.Environment qualified as Environment

import Lib qualified

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- IO.Utf8.readFile inputFile
  either (error . show) Lib.solve $ Lib.parse input
