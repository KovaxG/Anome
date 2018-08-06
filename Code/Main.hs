module Main where

import Utils

-- First classifier, we just have a lower and upper cutoff value
normalBounds :: (Double, Double) -> Classifier Double
normalBounds (lowerBound, upperBound) value _ = toClassification isBetweenBounds
  where
    isBetweenBounds = lowerBound < value && value < upperBound

main :: IO ()
main = putStrLn "Nothing to see here..."
