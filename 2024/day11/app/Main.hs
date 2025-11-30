import System.Environment qualified as Environment

import Lib qualified

main :: IO ()
main = do
  [inputFile] <- Environment.getArgs
  input <- readFile inputFile
  Lib.solve $ Lib.parse input
