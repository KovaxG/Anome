import Proposed_Counting
import Proposed_Distance
import Proposed_Weighted
import Traditional
import Types
import Utils

main :: IO ()
main = sequence_ (runExample <$> examples)

runExample :: Example -> IO ()
runExample (Example name c0 c1 c2) = do
  putStrLn $ replicate 60 '-'
  putStrLn $ "Name: " ++ name
  putStrLn $ "C0: " ++ show (length c0)
  putStrLn $ "C1: " ++ show (length c1)
  putStrLn $ "C2: " ++ show (length c2)
  showMetric "confMatrix"         confMatrix
  showMetric "acc"                m_acc
  showMetric "precision"          m_precision
  showMetric "recall"             m_recall
  showMetric "distance"           distance_m
  showMetric "countingWindow"     (countingWindow 10)
  showMetric "total detected"     (m_total_detected 10)
  showMetric "detection accuracy" (m_detection_accuracy 10)
  showMetric "linear weighted"    (weightedWindow (linear 10.0))
  showMetric "plinear weighted"   (weightedWindow (plinear 10.0))
  where
    showMetric :: Show a => String -> Metric a -> IO ()
    showMetric name m = do
      putStrLn ""
      putStrLn $ name ++ "(C1) = " ++ show (m c0 c1)
      putStrLn $ name ++ "(C2) = " ++ show (m c0 c2)

examples :: [Example]
examples = [
  Example {
    name = "Detection",
    c0 = normals 50 ++ anomaly ++ normals 50,
    c1 = normals 50 ++ anomaly ++ normals 50,
    c2 = normals 101
  },
  Example {
    name = "False Detection",
    c0 = normals 101,
    c1 = normals 101,
    c2 = normals 50 ++ anomaly ++ normals 50
  },
  Example {
    name = "Less Wrong",
    c0 = normals 50 ++ anomaly ++ normals 50,
    c1 = normals 50 ++ anomaly ++ normals 50,
    c2 = normals 50 ++ anomaly ++ normals 30 ++ anomaly ++ normals 19
  },
  Example {
    name = "Near Detection",
    c0 = normals 50 ++ anomaly ++ normals 50,
    c1 = normals 60 ++ anomaly ++ normals 40,
    c2 = normals 101
  },
  Example {
    name = "Closeness",
    c0 = normals 50 ++ anomaly ++ normals 50,
    c1 = normals 60 ++ anomaly ++ normals 40,
    c2 = normals 70 ++ anomaly ++ normals 30
  },
  Example {
    name = "Less Right vs More Wrong",
    c0 = normals 38 ++ anomaly ++ normals  5 ++ anomaly ++ normals  5 ++ anomaly ++ normals  5 ++ anomaly ++ normals 5 ++ anomaly ++ normals 38,
    c1 = normals 38 ++ anomaly ++ normals 11 ++ anomaly ++ normals 11 ++ anomaly ++ normals 38,
    c2 = normals 10 ++ anomaly ++ normals 27 ++ anomaly ++ normals  5 ++ anomaly ++ normals  5 ++ anomaly ++ normals 5 ++ anomaly ++ normals 5 ++ anomaly ++ normals 38
  }
  ]
