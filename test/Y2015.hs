module Y2015 where

import ClassyPrelude
import Lib (getInput)
import Test.Tasty
import Test.Tasty.HUnit
import Y2015.Day01 (basement, calculateFloor)
import Y2015.Day02 (presentArea, presentAreaSum, presentRibbon, shortestPerimeter)

tests2015 :: TestTree
tests2015 =
  testGroup
    "2015"
    [ testCase "2015-01" $ do
        assertEqual "Floor 0" 0 $ calculateFloor "(())"
        assertEqual "Floor 3" 3 $ calculateFloor "((("
        input <- getInput "2015" "01"
        assertEqual "Part 1" 280 $ calculateFloor input
        assertEqual "Part 2" (Just 1797) $ basement input,
      testCase "2015-02" $ do
        assertEqual "2x3x4" (Just 58) $ presentArea "2x3x4"
        assertEqual "1x1x10" (Just 43) $ presentArea "1x1x10"
        input <- getInput "2015" "02"
        assertEqual "Part 1" (Just 1606483) $ presentAreaSum input
        assertEqual "ShortestPerimeter 2x3x4" (Just 34) $ shortestPerimeter "2x3x4"
        assertEqual "Part 2" (Just 3842356) $ presentRibbon input
    ]
