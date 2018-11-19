module Utils where

import Data.List
import Data.Either.Extra
import Text.Parsec

readCSV :: String -> [[String]]
readCSV =
    transpose
    . filter (/= [""])
    . fromRight []
    . parse rule "idk"
    where
        rule = sepBy line (char '\n')
        line = sepBy cols (char ',')
        cols = many alphaNum

slidingWindow :: [a] -> Int -> ([a] -> b) -> [b]
slidingWindow as len f = go as
  where
    go as
      | length as >= len = f (take len as) : go (tail as)
      | otherwise =  []
