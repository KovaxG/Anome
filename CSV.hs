module CSV (
  CSV (..),
  readCSVsFromDir,
  getCol,
  nameOf,
  csvToExample
) where

import Data.Either
import Data.List
import Data.Maybe
import System.Directory
import Text.Parsec

import Types
import Utils

data CSV = CSV String [(String, [Int])] deriving (Show)

getCol :: CSV -> String -> Maybe [Int]
getCol (CSV _ cs) name = fmap snd $ find ((==name) . fst) cs

nameOf :: CSV -> String
nameOf (CSV name _) = name

csvToExample :: CSV -> Example
csvToExample csv =
  Example {
    name = nameOf csv,
    c0 = fromJust groundTruth,
    cs = map fromJust methodResults
  }
  where
    groundTruth = orElse (getCol csv "orig") (getCol csv "label")
    methodNames = ["bdm0", "bdm1", "median", "linapprox", "FAR"]
    methodResults = map (getCol csv) methodNames

readCSVsFromDir :: String -> IO [CSV]
readCSVsFromDir dir = do
  files <- getDirectoryContents dir
             |> fmap (filter ((!='.') . head))
  traverse (readCSV dir) files

readCSV :: String -> String -> IO CSV
readCSV dir fileName = do
  contents <- readFile (dir ++ "/" ++ fileName)
  parseCSV fileName contents
    |> fromRight (CSV fileName [])
    |> return

parseCSV :: String -> String -> Either ParseError CSV
parseCSV fileName = parse rule "CSV" . unlines . map (filter (!='\r')) . lines
  where
    row = sepBy (many (noneOf ",\n")) (char ',')
    rule = do
      rows <- sepBy row (char '\n')
      transpose rows
        |> filter ((!="") . head)
        |> map (\c -> (head c, map read $ tail c))
        |> CSV fileName
        |> return
