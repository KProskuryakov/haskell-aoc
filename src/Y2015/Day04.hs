module Y2015.Day04 (minMd5, minMd56) where

import ClassyPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits (shiftR)
import qualified Crypto.Hash.MD5 as MD5

minMd5 :: ByteString -> Int
minMd5 str = minMd5' str 0

minMd5' :: ByteString -> Int -> Int
minMd5' str cur = if checkBin (BS.unpack $ BS.take 3 hashed) then cur else minMd5' str (cur + 1)
  where
    hashed = MD5.hash (str <> BSC.pack (show cur))

    checkBin :: [Word8] -> Bool
    checkBin [a, b, c] = a == 0 && b == 0 && shiftR c 4 == 0
    checkBin _ = False

minMd56 :: ByteString -> Int
minMd56 str = minMd56' str 0

minMd56' :: ByteString -> Int -> Int
minMd56' str cur = if checkBin (BS.unpack $ BS.take 4 hashed) then cur else minMd56' str (cur + 1)
  where
    hashed = MD5.hash (str <> BSC.pack (show cur))

    checkBin :: [Word8] -> Bool
    checkBin [a, b, c, d] = a == 0 && b == 0 && c == 0 && d /= 0
    checkBin _ = False