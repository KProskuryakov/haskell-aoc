module Lib
  ( getInput,
  )
where

import ClassyPrelude

getInput :: Text -> Text -> IO Text
getInput y d = readFileUtf8 $ unpack $ "inputs/" <> y <> "/" <> d <> "/input"