module Traditional where

import Types

m_acc :: Metric Double
m_acc c0 ci = fromIntegral (tp + tn) / fromIntegral (tp + tn + fp + fn)
  where
    (tp, tn, fp, fn) = confMatrix c0 ci

m_recall :: Metric Double
m_recall c0 ci = fromIntegral tp / fromIntegral (tp + fp)
  where
    (tp, _, fp, _) = confMatrix c0 ci

m_precision :: Metric Double
m_precision c0 ci = fromIntegral tp / fromIntegral (tp + fn)
  where
    (tp, _, _, fn) = confMatrix c0 ci

confMatrix :: Metric (Int, Int, Int, Int)
confMatrix c0 ci = foldl countUp (0,0,0,0) $ zip c0 ci
  where
    countUp (tp, tn, fp, fn) v = case v of
      (1, 1) -> (tp + 1, tn, fp, fn)
      (0, 0) -> (tp, tn + 1, fp, fn)
      (0, 1) -> (tp, tn, fp + 1, fn)
      (1, 0) -> (tp, tn, fp, fn + 1)
