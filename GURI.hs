import Data.List
import Data.Maybe

import CSV
import Proposed_Distance
import Proposed_Counting
import Proposed_Weighted
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
  showMetric "confMatrix"         confMatrix
  showMetric "acc"                m_acc
  showMetric "precision"          m_precision
  showMetric "recall"             m_recall
  showMetric "distance"           distance_m
  showMetric "countingWindow"     (countingWindow 10)
  showMetric "total detected"     (m_total_detected 10)
  showMetric "detection accuracy" (m_detection_accuracy 10)
  --showMetric "linear weighted"    (weightedWindow (linear 10.0))
  --showMetric "plinear weighted"   (weightedWindow (plinear 10.0))
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

csvToExample :: CSV -> Example
csvToExample csv =
  Example {
    name = nameOf csv,
    c0 = fromJust groundTruth,
    cs = map fromJust methodResults
  }
  where
    groundTruth = orElse (getCol csv "orig") (getCol csv "label")
    methodNames = ["bdm0", "bdm1", "median", "linapprox", "FAR"]
    methodResults = map (getCol csv) methodNames

writeTikz :: String -> Example -> IO ()
writeTikz dir ex = do
  let gt = tikz (c0 ex) 0.0 0
  let methods =
        zipWithIndex (cs ex)
        |> (=<<) (\(i, ci) -> tikz ci (- fromIntegral i) i)
  writeFile (dir ++  "/"  ++ name ex ++ ".txt") $ unlines (gt ++ methods)
  where
    lineLength = if length (c0 ex) > 500 then 10 else 6
    tikz = toTikz lineLength

toTikz :: Double -> Classification -> Double -> Int -> [String]
toTikz endx c y index = mainLine : map (toCommand . toCoord) indexes
  where
    indexes = map fst $ filter ((==1) . snd) $ zip [0 ..] c

    toCoord :: Int -> Double
    toCoord i = (endx * fromIntegral i) / (fromIntegral $ length c)

    toCommand :: Double -> String
    toCommand x = concat [
      "\\draw[thick] ",
      show (x, y + 0.2),
      " -- ",
      show (x, y - 0.2),
      ";"
      ]

    mainLine :: String
    mainLine = concat [
      "\\draw[thick] ",
      show (0, y),
      " node[anchor=east] {$C_", show index, "$: }",
      " -- ",
      show (endx, y),
      ";"
      ]
