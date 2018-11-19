module Imp where

secondOrder :: Double 
            -> Double 
            -> Double 
            -> Double 
            -> Double 
            -> (Double, Double, Double) 
secondOrder y4 y3 y2 y1 y0 = (a, b, c)
  where
    bnum = y0*y3 - y0*y4 - y1*y2 + y1*y4 + y2*y2 - y2*y4 - y2*y3 + y2*y4
    bden = y1*y3 - y1*y4 - y3*y3 - y2*y2 + y2*y4 + y3*y2
    b = bnum / bden
    a = (y1 - b*(y2 - y3) - y2) / (y3 - y4)
    c = y2 - a*y4 - b*y3

slidingWindow :: [a] -> Int -> ([a] -> b) -> [b]
slidingWindow as len f = go as
  where
    go as 
      | length as >= len = f (take len as) : go (tail as)
      | otherwise =  []
