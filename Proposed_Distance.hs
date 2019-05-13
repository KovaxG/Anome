module Proposed_Distance (distance_m) where

import Types
import Utils

distance_m :: Metric (Maybe Double)
distance_m c0 ci = do
  ctt <- closest c0 ci
  ttc <- closest ci c0
  return $ fromIntegral (ctt + ttc)

closest :: Metric (Maybe Int)
closest c0 ci
  | null c0' || null ci' = Nothing
  | otherwise = Just $ sum $ map (\i -> minimum $ map (\j -> abs $ i - j) ci') c0'
  where
    c0' = map fst $ filter ((==1) . snd) (zipWithIndex c0)
    ci' = map fst $ filter ((==1) . snd) (zipWithIndex ci)
