module Y2021.Day02 (dive1, dive2) where

import ClassyPrelude hiding (Down)
import Data.Text (splitOn)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.List (foldl)

data Dir = Forward Int | Down Int | Up Int

parse :: Text -> Maybe [Dir]
parse ls = mapM parseDir splits
  where
    parseDir :: [Text] -> Maybe Dir
    parseDir ["forward", i] = Forward <$> parseToInt i
    parseDir ["down", i] = Down <$> parseToInt i
    parseDir ["up", i] = Up <$> parseToInt i
    parseDir _ = Nothing

    parseToInt :: Text -> Maybe Int
    parseToInt i = readMaybe (T.unpack i) :: Maybe Int

    splits = map (splitOn " ") $ lines ls

processDir :: Dir -> (Int, Int) -> (Int, Int)
processDir (Forward i) (x, y) = (x + i, y)
processDir (Down i) (x, y) = (x, y + i)
processDir (Up i) (x, y) = (x, y - i)

dive1 :: Text -> Maybe Int
dive1 ls = uncurry (*) . foldl (flip processDir) (0, 0) <$> parse ls

processDir2 :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
processDir2 (x, y, a) (Forward i) = (x + i, y + a * i, a)
processDir2 (x, y, a) (Down i) = (x, y, a + i)
processDir2 (x, y, a) (Up i) = (x, y, a - i)

dive2 :: Text -> Maybe Int
dive2 ls = (\(a,b,_) -> a * b) . foldl processDir2 (0, 0, 0) <$> parse ls