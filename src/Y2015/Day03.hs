module Y2015.Day03
  ( multiDeliveries,
    roboDeliveries
  )
where

import ClassyPrelude

multiDeliveries :: Text -> Int
multiDeliveries ls = length $ group $ sort $ multi' (unpack ls) (0, 0) []
  where
    multi' :: String -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    multi' (d:rest) cur visited = multi' rest (dir d cur) (cur : visited)
    multi' "" cur visited = cur : visited

dir :: Char -> (Int, Int) -> (Int, Int)
dir '^' (a, b) = (a, b + 1)
dir '>' (a, b) = (a + 1, b)
dir 'v' (a, b) = (a, b - 1)
dir '<' (a, b) = (a - 1, b)
dir _ v = v

roboDeliveries :: Text -> Int
roboDeliveries ls = length $ group $ sort $ robo' (unpack ls) (0,0) (0,0) []
  where
    robo' :: String -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    robo' (a:b:rest) cur1 cur2 visited = robo' rest (dir a cur1) (dir b cur2) (cur1 : cur2 : visited)
    robo' [a] cur1 cur2 visited = robo' "" (dir a cur1) cur2 (cur1 : cur2 : visited)
    robo' "" cur1 cur2 visited = cur1 : cur2 : visited
