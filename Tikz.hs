module Tikz (
  writeTikz
) where

import Types
import Utils

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
