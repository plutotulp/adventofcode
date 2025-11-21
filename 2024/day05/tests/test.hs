{-# language OverloadedStrings #-}

import Data.Text (Text)
import Data.Text.IO.Utf8 qualified as IO.Utf8
import GHC.IO qualified
import System.Environment qualified as Environment
import Test.Hspec.Megaparsec qualified as THM
import Test.Tasty qualified as Tasty
import Test.Tasty.Hspec qualified as TH
import Text.Megaparsec qualified as Megaparsec

import Lib qualified

import Test.Hspec

main :: IO ()
main = do
  ls <-TH.testSpec "Lib spec" libSpec
  Tasty.defaultMain (Tasty.testGroup "Unit tests" [ ls ])

libSpec = do

  describe "parseRule" $ do
    it "parse a rule" $ do
      THM.shouldParse (Megaparsec.runParser Lib.parseRule "" "32|84") (32 `Lib.Before` 84)

  describe "parseUpdate" $ do
    it "parse an update" $ do
      THM.shouldParse (Megaparsec.runParser Lib.parseUpdate "" "32,84,11,33,55") [32,84,11,33,55]
