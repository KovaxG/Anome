module Types where

type Classification = [Int]
type Metric a = Classification -> Classification -> a

data Example = Example {
  name :: String,
  c0 :: Classification,
  cs :: [Classification]
} deriving (Show)

myShow :: Classification -> String
myShow = map rule
  where
    rule 1 = '|'
    rule 0 = '-'

myRead :: String -> Classification
myRead = map rule
  where
    rule '|' = 1
    rule '-' = 0
