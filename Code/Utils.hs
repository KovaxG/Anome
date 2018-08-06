module Utils (
  Classification (..),
  Classifier,
  toClassification
) where

data Classification = Normal
                    | Anomaly
                    deriving (Show)

type Classifier p = p -> [p] -> Classification

toClassification :: Bool -> Classification
toClassification isNormal = if isNormal then Normal else Anomaly
