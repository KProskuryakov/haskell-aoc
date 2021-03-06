module Y2021 (tests2021) where

import ClassyPrelude
import Lib (getInput, linesInput)
import Test.Tasty
import Test.Tasty.HUnit

import Y2021.Day01 (sonarSweep1, sonarSweep2)
import Y2021.Day02 (dive1, dive2)

tests2021 :: TestTree
tests2021 =
  testGroup
    "2021"
    [ testCase "2021-01" $ do
        input <- linesInput "2021" "01"
        assertEqual "Test 1" (Just 1559) $ sonarSweep1 input
        assertEqual "Test 2" (Just 1600) $ sonarSweep2 input,
      testCase "2021-02"$ do
        input <- getInput "2021" "02"
        assertEqual "Test 1" (Just 1746616) $ dive1 input
        assertEqual "Test 2" (Just 1741971043) $ dive2 input
    ]