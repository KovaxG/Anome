module Utils where

type Index = Int

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [1 ..]

normals :: Int -> [Int]
normals n = replicate n 0

anomaly :: [Int]
anomaly = [1]

get :: [a] -> Index -> a
get as i = as !! (i-1)
