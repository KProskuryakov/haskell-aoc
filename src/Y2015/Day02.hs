module Y2015.Day02
  ( presentArea,
    presentAreaSum,
    presentRibbon,
    shortestPerimeter
  )
where

import ClassyPrelude
import Data.Text (split)
import Text.Read (readMaybe)

presentArea :: Text -> Maybe Int
presentArea ls = area =<< dims ls
  where
    area :: [Int] -> Maybe Int
    area [a, b, c] = Just $ (a * b * 2) + (a * c * 2) + (b * c * 2) + min (a * b) (min (a * c) (b * c))
    area _ = Nothing

dims :: Text -> Maybe [Int]
dims ls = mapM readAsInt (split (== 'x') ls)
  where
    readAsInt :: Text -> Maybe Int
    readAsInt = readMaybe . unpack

presentAreaSum :: Text -> Maybe Int
presentAreaSum = fmap sum . mapM presentArea . lines

shortestPerimeter :: Text -> Maybe Int
shortestPerimeter ls = getShortestThings =<< dims ls
  where
    getShortestThings :: [Int] -> Maybe Int
    getShortestThings ls@[_, _, _] = Just (double (sum $ take 2 $ sort ls) + product ls)
    getShortestThings _ = Nothing

    double a = a * 2

presentRibbon :: Text -> Maybe Int
presentRibbon = fmap sum . mapM shortestPerimeter . lines