module Utils (
  Classification (..),
  Classifier,
  anomalyIf
) where

data Classification = Normal
                    | Anomaly
                    deriving (Show)

type Classifier p = p -> [p] -> Classification

anomalyIf :: Bool -> Classification
anomalyIf isNormal = if isNormal then Normal else Anomaly
