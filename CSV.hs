module CSV (
  CSV (..),
  readCSVsFromDir,
  getCol,
  nameOf
) where

import Data.Either
import Data.List
import System.Directory
import Text.Parsec

import Utils

data CSV = CSV String [(String, [Int])] deriving (Show)

getCol :: CSV -> String -> Maybe [Int]
getCol (CSV _ cs) name = fmap snd $ find ((==name) . fst) cs

nameOf :: CSV -> String
nameOf (CSV name _) = name

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