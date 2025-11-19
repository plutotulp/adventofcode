{-# language TemplateHaskell #-}

import Data.FileEmbed qualified as FileEmbed
import Data.Text (Text)

import Lib qualified

inputFile :: Text
inputFile = $(FileEmbed.embedStringFile "input")

main :: IO ()
main = either (error . show) Lib.solve $ Lib.parse inputFile
