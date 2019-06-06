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

(|>) :: a -> (a -> b) -> b
a |> f = f a

a != b = a /= b

orElse :: Maybe a -> Maybe a -> Maybe a
orElse ma mb = maybe mb Just ma
