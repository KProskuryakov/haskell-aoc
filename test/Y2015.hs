module Y2015 where

import ClassyPrelude
import Lib (getInput)
import Test.Tasty
import Test.Tasty.HUnit
import Y2015.Day01 (basement, calculateFloor)
import Y2015.Day02 (presentArea, presentAreaSum, presentRibbon, shortestPerimeter)
import Y2015.Day03 (multiDeliveries, roboDeliveries)
import Y2015.Day04 (minMd5, minMd56)
import Y2015.Day05 (niceWord, niceWords, niceWord2, niceWords2)

tests2015 :: TestTree
tests2015 =
  testGroup
    "2015"
    [ testCase "2015-01" $ do
        assertEqual "Floor 0" 0 $ calculateFloor "(())"
        assertEqual "Floor 3" 3 $ calculateFloor "((("
        input <- getInput "2015" "01"
        assertEqual "Part 1" 280 $ calculateFloor input
        -- part 2
        assertEqual "Part 2" (Just 1797) $ basement input,
      testCase "2015-02" $ do
        assertEqual "2x3x4" (Just 58) $ presentArea "2x3x4"
        assertEqual "1x1x10" (Just 43) $ presentArea "1x1x10"
        input <- getInput "2015" "02"
        assertEqual "Part 1" (Just 1606483) $ presentAreaSum input
        -- part 2
        assertEqual "ShortestPerimeter 2x3x4" (Just 34) $ shortestPerimeter "2x3x4"
        assertEqual "Part 2" (Just 3842356) $ presentRibbon input,
      testCase "2015-03" $ do
        assertEqual ">" 2 $ multiDeliveries ">"
        assertEqual "^>v<" 4 $ multiDeliveries "^>v<"
        assertEqual "^v^v^v^v^v" 2 $ multiDeliveries "^v^v^v^v^v"
        input <- getInput "2015" "03"
        assertEqual "Part 1" 2565 $ multiDeliveries input
        -- part 2
        assertEqual "^v" 3 $ roboDeliveries ">v"
        assertEqual "^>v<" 3 $ roboDeliveries "^>v<"
        assertEqual "^v^v^v^v^v" 11 $ roboDeliveries "^v^v^v^v^v"
        assertEqual "Part 2" 2639 $ roboDeliveries input,
      testCase "2015-04" $ do
        assertEqual "abcdef" 609043 $ minMd5 "abcdef"
        assertEqual "pqrstuv" 1048970 $ minMd5 "pqrstuv"
        assertEqual "iwrupvqb" 346386 $ minMd5 "iwrupvqb"
        -- part 2
        assertEqual "iwrupvqb 2" 9958218 $ minMd56 "iwrupvqb",
      testCase "2015-05" $ do
        assertEqual "ugknbfddgicrmopn" True $ niceWord "ugknbfddgicrmopn"
        assertEqual "aaa" True $ niceWord "aaa"
        assertEqual "jchzalrnumimnmhp" False $ niceWord "jchzalrnumimnmhp"
        assertEqual "haegwjzuvuyypxyu" False $ niceWord "haegwjzuvuyypxyu"
        assertEqual "dvszwmarrgswjxmb" False $ niceWord "dvszwmarrgswjxmb"
        input <- getInput "2015" "05"
        assertEqual "part 1" 258 $ niceWords input
        -- part 2
        assertEqual "2 - qjhvhtzxzqqjkmpb" True $ niceWord2 "qjhvhtzxzqqjkmpb"
        assertEqual "2 - uurcxstgmygtbstg" False $ niceWord2 "uurcxstgmygtbstg"
        assertEqual "part 2" 53 $ niceWords2 input
    ]

