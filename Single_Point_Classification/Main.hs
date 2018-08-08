module Main where

import Utils

normalBounds :: (Double, Double) -> Classifier Double
normalBounds (lowerBound, upperBound) value _ =
  anomalyIf . not $ isBetweenBounds
  where
    isBetweenBounds = lowerBound < value && value < upperBound

main :: IO ()
main = putStrLn "Nothing to see here..."
