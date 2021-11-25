{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Lib (getInput)
import Test.Tasty
import Test.Tasty.HUnit
import Y2015.Day01 (basement, calculateFloor)
import Y2015.Day02 (presentAreaSum, presentArea)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All Tests" [tests2015]

tests2015 :: TestTree
tests2015 =
  testGroup
    "2015"
    [ testCase "2015-01" $ do
        assertEqual "Floor 0" 0 $ calculateFloor "(())"
        assertEqual "Floor 3" 3 $ calculateFloor "((("
        input <- getInput "2015" "01"
        assertEqual "Part 1" 280 $ calculateFloor input
        assertEqual "Part 2" (Just 1797) $ basement input
    , testCase "2015-02" $ do
        assertEqual "2x3x4" (Just 58) $ presentArea "2x3x4"
        assertEqual "1x1x10" (Just 43) $ presentArea "1x1x10"
        input <- getInput "2015" "02"
        assertEqual "Part 1" (Just 1606483) $ presentAreaSum input
    ]
