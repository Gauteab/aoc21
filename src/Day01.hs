module Day01 (
  main,
) where

import Control.Lens

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input/01"
  print . length . filter (uncurry (<)) . (zip <*> tail) $ input
  print . length . filter (uncurry (<)) . (zip <*> tail) . map (sum . toListOf each) . (zip3 <*> tail <*> drop 2) $ input
