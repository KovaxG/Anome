module Proposed_Distance (
  distance_m,
  scale,
  square
) where

import Types
import Utils

scale :: Double -> Double -> Double
scale a x = a * x

square :: Double -> Double
square x = x^2

distance_m :: (Double -> Double) -> Metric (Maybe Double)
distance_m f c0 ci = do
  ctt <- closest f c0 ci
  ttc <- closest f ci c0
  return (ctt + ttc)

closest :: (Double -> Double) -> Metric (Maybe Double)
closest f c0 ci
  | null c0' || null ci' = Nothing
  | otherwise = Just $ sum $ map (\i -> minimum $ map (distance i) ci') c0'
  where
    distance x y = f $ fromIntegral $ abs (x - y)
    c0' = map fst $ filter ((==1) . snd) (zipWithIndex c0)
    ci' = map fst $ filter ((==1) . snd) (zipWithIndex ci)
