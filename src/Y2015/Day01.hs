module Y2015.Day01
  ( calculateFloor,
    basement,
  )
where

import ClassyPrelude
import Data.List (scanl)

-- map parens to 1/-1 then sum them up
calculateFloor :: Text -> Int
calculateFloor = sum . map toNum . unpack

toNum :: Char -> Int
toNum '(' = 1
toNum ')' = -1
toNum _ = 0

-- use scan to create a list of all floors visited in order, zip with index
-- filter out everything but -1 floors, grab the index then subtract 1 to get the right result
basement :: Text -> Maybe Int
basement s = flip (-) 1 . fst <$> find (\(_, b) -> b == - 1) (zip [1 ..] ls)
  where
    ls = scanl (+) 0 $ map toNum $ unpack s