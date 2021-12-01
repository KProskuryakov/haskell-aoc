module Y2021.Day01 
  ( sonarSweep1,
    sonarSweep2
  )
where

import ClassyPrelude

import Text.Read (readMaybe)

stringToInts :: [String] -> Maybe [Int]
stringToInts = mapM (\s -> readMaybe s :: Maybe Int)

sonarSweep1 :: [String] -> Maybe Int
sonarSweep1 ls =
  do 
    ints <- stringToInts ls
    let signs = zipWith (\a b -> signum (b - a)) ints $ drop 1 ints
    Just $ sum $ filter (== 1) signs

sonarSweep2 :: [String] -> Maybe Int
sonarSweep2 ls =
  do
    ints <- stringToInts ls
    let trips = zipWith3 (\a b c -> a + b + c) ints (drop 1 ints) (drop 2 ints)
    let signs = zipWith (\a b -> signum (b - a)) trips $ drop 1 trips
    Just $ sum $ filter (== 1) signs