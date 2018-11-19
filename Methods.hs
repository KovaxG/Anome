module Methods where

type Point = (Double, Double)

-- distance of the third point of the line made up of the first two points
distanceToLine :: Point -> Point -> Point -> Double
distanceToLine (x1, y1) (x2, y2) (xp, yp) = d
  where
    m = (y2 - y1) / (x2 - x1)
    b = m * x1 + y1
    d = abs (m * xp - yp + b) / sqrt (m^2 + 1)

firstOrder :: Double -> Double -> Double -> (Double, Double)
firstOrder y0 y1 y2 = (a, b)
  where
      a = (y0 - y1) / (y1 - y2)
      b = y1 - a * y2

firstOrder2 :: Double -> Double -> Double -> (Double, Double)
firstOrder2 y0 y1 y2 = (a, b)
  where
      a = (y1 - b) / y2
      b = (y0 * y2 - y1 * y1) / (y2 - y1)

secondOrder :: Double -> Double -> Double -> Double -> Double -> (Double, Double, Double)
secondOrder y4 y3 y2 y1 y0 = (a, b, c)
  where
    bnum = y0*y3 - y0*y4 - y1*y2 + y1*y4 + y2*y2 - y2*y4 - y2*y3 + y2*y4
    bden = y1*y3 - y1*y4 - y3*y3 - y2*y2 + y2*y4 + y3*y2
    b = bnum / bden
    a = (y1 - b*(y2 - y3) - y2) / (y3 - y4)
    c = y2 - a*y4 - b*y3
