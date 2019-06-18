module Gensler (
  adc,
  add
) where

import Types
import Utils

import Debug.Trace

adc :: Int -> Metric Int
adc sz c0 ci = sum candidateValues
  where
    candidateValues =
      zipWithIndex ci
      |> filter (\(i, _) -> elem i zoneIndexes)
      |> map snd

    zoneIndexes =
      zipWithIndex c0
      |> filter (\(_, v) -> v == 1)
      |> map fst
      |> (=<<) (range sz)

add :: Int -> Metric (Maybe Int)
add sz c0 ci
  | null anomalyIndexes || null candidateIndexes = Nothing
  | otherwise = Just $ sum distancesToAnomalies
  where
    distancesToAnomalies =
      candidateIndexes
      |> map (\ci -> minimum $ map (\ai -> abs (ai - ci)) anomalyIndexes)
      |> filter (\d -> d <= sz)

    candidateIndexes =
      zipWithIndex ci
      |> filter (\(_, v) -> v == 1)
      |> map fst

    anomalyIndexes =
      zipWithIndex c0
      |> filter (\(_, v) -> v == 1)
      |> map fst

range :: Int -> Int -> [Int]
range r i = [i-r .. i+r]
