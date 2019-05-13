module Proposed_Counting (
  countingWindow,
  m_total_detected,
  m_detection_accuracy
) where

import Types
import Utils

m_total_detected :: Int -> Metric Double
m_total_detected w c0 ci = focus $ toDoubles $ countingWindow w c0 ci
  where
    focus (em, ad, am, _) = (em + ad) / (em + ad + am)

m_detection_accuracy :: Int -> Metric Double
m_detection_accuracy w c0 ci = focus $ toDoubles $ countingWindow w c0 ci
  where
    focus (em, ad, _, fa) = (em + ad) / (em + ad + fa)

-- (Exact Match, Anomaly Detected, Anomaly Missed, False Anomaly)
countingWindow :: Int -> Metric (Int, Int, Int, Int)
countingWindow w c0 ci = foldl rule (0,0,0,0) $ windowedList w c0 ci
  where
    rule t@(em, ad, am, fa) (c, Zip l m r)
      | c == 1 && m == 1                          = (em + 1, ad, am, fa)
      | c == 1 && elem 1 (l ++ r)                 = (em, ad + 1, am, fa)
      | c == 1 && m == 0 && not (elem 1 (l ++ r)) = (em, ad, am + 1, fa)
      | c == 0 && m ==1                           = (em, ad, am, fa + 1)
      | otherwise = t

-- Here is an example of how it works
-- windowedList 1 [1,2,3,4] [5,6,7,8] = [(1, [(5),6]), (2, [5,(6),7]), (3, [6,(7),8]), (4, [7,(8)])]
-- where the middle value is indicated with round brackets
windowedList :: Int -> [a] -> [a] -> [(a, Zip a)]
windowedList n as bs = map (tupleBiMap id (fmap (get bs) . around)) $ withIndex as
  where
    around :: Index -> Zip Index
    around i = Zip (bounded [i-n..i-1]) i (bounded [i+1..i+n])

    bounded :: [Index] -> [Index]
    bounded = filter (\x -> x > 0 && x <= length as)

withIndex :: [a] -> [(a, Index)]
withIndex as = zip as [1..]

data Zip a = Zip [a] a [a] deriving (Show)

tupleBiMap :: (a1 -> a2) -> (b1 -> b2) -> (a1, b1) -> (a2, b2)
tupleBiMap fa fb (a, b) = (fa a, fb b)

tupleQuaMap :: (a1 -> a2) -> (b1 -> b2) -> (c1 -> c2) -> (d1 -> d2) -> (a1, b1, c1, d1) -> (a2, b2, c2, d2)
tupleQuaMap fa fb fc fd (a, b, c, d) = (fa a, fb b, fc c, fd d)

toDoubles :: (Int, Int, Int, Int) -> (Double, Double, Double, Double)
toDoubles = tupleQuaMap fromIntegral fromIntegral fromIntegral fromIntegral

instance Functor Zip where
  fmap f (Zip l m r) = Zip (f <$> l) (f m) (f <$> r)
