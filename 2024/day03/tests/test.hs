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

parsedExampleFile1 :: [Lib.Instr]
parsedExampleFile1 = [Lib.Mul 2 4, Lib.Mul 5 5, Lib.Mul 11 8, Lib.Mul 8 5]

parsedExampleFile2 :: [Lib.Instr]
parsedExampleFile2 =
  [ Lib.Mul 2 4
  , Lib.Dont
  , Lib.Mul 5 5
  , Lib.Mul 11 8
  , Lib.Do
  , Lib.Mul 8 5
  ]

libSpec = do

  describe "parseMul" $ do
    it "parses text containing only a single mul instruction" $ do
      THM.shouldParse (Megaparsec.runParser Lib.parseMul "" "mul(3,4)") (Lib.Mul 3 4)

  describe "parse" $ do
    it "parses a mul instruction prefixed by a noise character" $ do
      THM.shouldParse (Lib.parse "xmul(3,4)") [Lib.Mul 3 4]

  describe "sumOfMuls" $ do
    it "calculates sum om mul instructions from part 1" $ do
      Lib.sumOfMuls parsedExampleFile1 `shouldBe` 161
    it "calculates sum om mul instructions from part 2" $ do
      Lib.sumOfEnabledMuls parsedExampleFile2 `shouldBe` 48
