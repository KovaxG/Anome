import CSV
import Proposed_Distance
import Proposed_Counting
import Proposed_Weighted
import Gensler
import Tikz
import Traditional
import Types
import Utils

main :: IO ()
main = do
  csvs <- readCSVsFromDir "Data"
  let csvExamples = map csvToExample csvs
  let allExamples = examples ++ csvExamples
  sequence_ ((\e -> writeTikz "Tikz" e >> runExample e) <$> allExamples)

runExample :: Example -> IO ()
runExample (Example name c0 cs) = do
  putStrLn $ replicate 60 '-'
  putStrLn $ "Name: " ++ name
  putStrLn $ "C0: " ++ show (length c0)
  mapM_ putStrLn $ map (\(i, ci) -> "C" ++ show i ++ ": " ++ show (length ci)) csi
  showMetric "confMatrix"     confMatrix
  showMetric "acc"            m_acc
  showMetric "precision"      m_precision
  showMetric "recall"         m_recall
  showMetric "TD"             (distance_m (scale 1))
  showMetric "STD"            (distance_m square)
  showMetric "countingWindow" (countingWindow 10)
  showMetric "TDIR"           (m_total_detected 10)
  showMetric "DAIR"           (m_detection_accuracy 10)
  showMetric "weightedwindow" (weightedWindow (linear 10.0))
  showMetric "WDD"            (wdd (linear 10.0) 0.6)
  showMetric "ADC"            (adc 10)
  showMetric "ADD"            (add 10)
  where
    csi :: [(Int, Classification)]
    csi = zipWithIndex cs

    showMetric :: Show a => String -> Metric a -> IO ()
    showMetric name m = do
      putStrLn ""
      mapM_ putStrLn $ map (\(i, ci) -> name ++ "(C" ++ show i ++ ") = " ++ show (m c0 ci)) csi

examples :: [Example]
examples = [
  Example {
    name = "Detection",
    c0 = normals 50 ++ anomaly ++ normals 50,
    cs = [
      normals 50 ++ anomaly ++ normals 50,
      normals 101
      ]
  },
  Example {
    name = "False Detection",
    c0 = normals 101,
    cs = [
      normals 101,
      normals 50 ++ anomaly ++ normals 50
      ]
  },
  Example {
    name = "Less Wrong",
    c0 = normals 50 ++ anomaly ++ normals 50,
    cs = [
      normals 50 ++ anomaly ++ normals 50,
      normals 50 ++ anomaly ++ normals 30 ++ anomaly ++ normals 19
      ]
  },
  Example {
    name = "Near Detection",
    c0 = normals 50 ++ anomaly ++ normals 50,
    cs = [
      normals 60 ++ anomaly ++ normals 40,
      normals 101
      ]
  },
  Example {
    name = "Closeness",
    c0 = normals 50 ++ anomaly ++ normals 50,
    cs = [
      normals 60 ++ anomaly ++ normals 40,
      normals 70 ++ anomaly ++ normals 30
      ]
  },
  Example {
    name = "Less Right vs More Wrong",
    c0 = normals 72 ++ anomaly ++ normals 5 ++ anomaly ++ normals 4 ++ anomaly ++ normals 17,
    cs = [
      normals 72 ++ anomaly ++ normals 10 ++ anomaly ++ normals 17,
      normals 11 ++ anomaly ++ normals 60 ++ anomaly ++ normals 5 ++ anomaly ++ normals 4 ++ anomaly ++ normals 17
      ]
  }
  ]
