import ClassyPrelude
import Lib (getInput)
import Test.Tasty
import Test.Tasty.HUnit
import Y2015 (tests2015)
import Y2021 (tests2021)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
-- allTests = testGroup "All Tests" [tests2015]
allTests = testGroup "All Tests" [tests2021]
