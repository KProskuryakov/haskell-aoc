module Y2015.Day05 (niceWord, niceWords, niceWord2, niceWords2) where

import ClassyPrelude
import Data.Text (splitOn)

niceWord :: String -> Bool
niceWord s = all ($ s) [has3Vowels, hasDouble, noBadPairs]

has3Vowels :: String -> Bool
has3Vowels = (>= 3) . length . filter (`elem` ("aeiou" :: String))

hasDouble :: String -> Bool
hasDouble = any ((>= 2) . length) . group

noBadPairs :: String -> Bool
noBadPairs s = not $ any (contains s) ["ab", "cd", "pq", "xy"]

niceWords :: Text -> Int
niceWords = length . filter (niceWord . unpack) . lines

trip :: String -> [(Char, Char, Char)]
trip (a:b:c:ls) = (a, b, c) : trip (b : c : ls)
trip _ = []

contains :: String -> String -> Bool
contains s c = (>= 2) $ length $ (`splitOn` pack s) $ pack c

niceWord2 :: String -> Bool
niceWord2 s = all ($ s) [tripRepeat, pairRepeat]

tripRepeat :: String -> Bool
tripRepeat = any (\(a, _, c) -> a == c) . trip

pairRepeat :: String -> Bool
pairRepeat (a:b:ls) = contains ls [a, b] || pairRepeat (b : ls)
pairRepeat _ = False

niceWords2 :: Text -> Int
niceWords2 = length . filter (niceWord2 . unpack) . lines