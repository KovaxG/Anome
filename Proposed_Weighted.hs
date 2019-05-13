module Proposed_Weighted (
  weightedWindow,
  linear,
  plinear
) where

import Data.Maybe

import Types
import Utils

type Distance = Double
type Weight = Double


linear :: Double -> (Distance -> Weight)
linear dmax = \d -> 1 - (d / dmax)

plinear :: Double -> (Distance -> Weight)
plinear dmax = \d -> max 0 $ linear dmax d

data Data = Data Double Int Int deriving (Show)

instance Semigroup Data where
  (Data d1 m1 f1) <> (Data d2 m2 f2) = Data (d1 + d2) (m1 + m2) (f1 + f2)

instance Monoid Data where mempty = Data 0 0 0

tuplefy :: Data -> (Double, Int, Int)
tuplefy (Data d m f) = (d, m, f)

weightedWindow :: (Distance -> Weight) -> Metric (Double, Int, Int)
weightedWindow f c0 ci = tuplefy $ mconcat $ map detect c0'
  where
    detect :: (Index, Int) -> Data
    detect (i, v)
      | v == 0 && get ci i == 1 = Data 0 0 1
      | v == 1 && isNothing val = Data 0 1 0
      | v == 1 && isJust val = Data (fromJust val) 0 0
      | otherwise = mempty
      where
        val = f <$> distanceToClosestAnomaly ci' i

    c0' = zipWithIndex c0
    ci' = map fst $ filter ((==1) . snd) $ zipWithIndex ci

distanceToClosestAnomaly :: [Index] -> Index -> (Maybe Distance)
distanceToClosestAnomaly is i
  | null is = Nothing
  | otherwise = Just $ fromIntegral $ minimum $ map (abs . (-)i) is
