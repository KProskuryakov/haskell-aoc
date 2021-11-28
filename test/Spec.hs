import ClassyPrelude
import Lib (getInput)
import Test.Tasty
import Test.Tasty.HUnit
import Y2015 (tests2015)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All Tests" [tests2015]
