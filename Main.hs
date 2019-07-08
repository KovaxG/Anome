import CSV
import Proposed_Distance
import Proposed_Counting
import Proposed_Weighted
import Gensler
import Tikz
import Traditional
import Types
import Utils

import Data.List
import Data.Maybe
import Text.Printf

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

    showMetric :: (Show a, GyuriShow a) => String -> Metric a -> IO ()
    showMetric name m = do
      putStrLn ""
      mapM_ putStrLn $ map (\(i, ci) -> name ++ "(C" ++ show i ++ ") = " ++ show (m c0 ci)) csi
      putStrLn $ concat $ intersperse " & " $ map put $ map (m c0) cs

class GyuriShow a where
    put :: a -> String
    
instance GyuriShow Double where
    put = printf "%.4f"
      
instance GyuriShow Int where
    put = printf "%d"
    
instance (GyuriShow a, GyuriShow b, GyuriShow c) => GyuriShow (a, b, c) where
    put (a,b,c) = "(" ++ put a ++ ", " ++ put b ++ ", " ++ put c ++ ")"
      
instance (GyuriShow a, GyuriShow b, GyuriShow c, GyuriShow d) => GyuriShow (a, b, c, d) where
    put (a,b,c,d) = "(" ++ put a ++ ", " ++ put b ++ ", " ++ put c ++ "," ++ put d ++ ")"
      
instance GyuriShow a => GyuriShow (Maybe a) where
    put (Just a) = put a
    put Nothing = "-"
      
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
  
dd :: [Int] -> [Int] -> Int
dd as bs = sum $ zipWith d as bs
  where
    d :: Int -> Int -> Int
    d a b = abs (a - b)
    
data Order = Bigger | Smaller deriving (Show)
    
scores1 = [
    score Bigger [4,3,1,2,5] "0.9449  & 0.9149  & 0.9049  & 0.9069  & 0.9059",
    score Bigger [4,3,1,2,5] "0.3678  & 0.0689  & 0.0919  & 0.0344  & 0.0149",
    score Bigger [4,3,1,2,5] "1       & 0.6     & 0.3333  & 0.25    & 0.1111",
    score Bigger [4,3,1,2,5] "32      & 9       & 11      & 10      & 3",
    score Smaller [4,3,1,2,5] "0       & 3       & 23      & 11      & 16",
    score Smaller [4,3,1,2,5] "2706    & 1322    & 1192    & 1221    & 4494",
    score Smaller [4,3,1,2,5] "310650  & 37754   & 31468   & 33737   & 336162",
    score Bigger [4,3,1,2,5] "30.4942  & 0.4252  & 0.4712  & 0.4827  & 0.1724", 
    score Bigger [4,3,1,2,5] "1       & 0.9024  & 0.7192  & 0.8235  & 0.6521",
    score Bigger [4,3,1,2,5] "-183.6  & -45.8   & -36.6   & -35.8   & -336.8",
    score Smaller [4,3,1,2,5] "4       & 3       & 1       & 2       & 5"
    ]
    
scores2 = [
    score Bigger [5,1,2,3,4] "0.7354  & 0.9609  & 0.9659  & 0.9619  & 0.9498",
    score Bigger [5,1,2,3,4] "0.9333  & 0.1555  & 0.4444  & 0.2     & 0.0222",
    score Bigger [5,1,2,3,4] "0.1386  & 0.875   & 0.6896  & 0.8181  & 0.1428",
    score Bigger [5,1,2,3,4] "92      & 8       & 29      & 10      & 5",
    score Smaller [5,1,2,3,4] "275     & 1       & 18      & 2       & 23",
    score Smaller [5,1,2,3,4] "8101    & 166     & 183     & 147     & 4123",
    score Smaller [5,1,2,3,4] "364933  & 1360    & 1849    & 3079    & 892305",
    score Bigger [5,1,2,3,4] "1       & 0.8888  & 0.8444  & 1       & 0.2", 
    score Bigger [5,1,2,3,4] "0.1470  & 0.9756  & 0.8085  & 0.9574  & 0.6",
    score Bigger [5,1,2,3,4] "-112.1999& 27.8999 & 23.0999 & 34.4999 & -353.6",
    score Smaller [5,1,2,3,4] "5       & 1       & 2       & 3       & 4"
    ]
    
score :: Order -> [Int] -> String -> Int
score ordr as s =  dd as $ map (indexOf $ order ordr) elems
  where
    order :: Order -> [Double]
    order Bigger = reverse $ sort elems
    order Smaller = sort elems
      
    elems :: [Double]
    elems = parse s
      
    parse :: String -> [Double]
    parse = map read . words . filter (/='&')
    
    indexOf :: [Double] -> Double -> Int
    indexOf as a = 1 + fromJust (elemIndex a as)
