module Y2015.Day02 where

import ClassyPrelude
import Data.Text (split)
import Text.Read (readMaybe)

presentArea :: Text -> Maybe Int
presentArea ls = area =<< dims (split (== 'x') ls)
  where
    area :: [Int] -> Maybe Int
    area [a, b, c] = Just $ (a * b * 2) + (a * c * 2) + (b * c * 2) + min (a * b) (min (a * c) (b * c))
    area _ = Nothing

dims :: [Text] -> Maybe [Int]
dims ls@[_, _, _] = mapM readAsInt ls
  where
    readAsInt :: Text -> Maybe Int
    readAsInt = readMaybe . unpack
dims _ = Nothing

presentAreaSum :: Text -> Maybe Int
presentAreaSum = fmap sum . mapM presentArea . lines