{-# LANGUAGE TypeApplications #-}

module Day01 (
  main,
) where

import Relude.Unsafe (fromJust)

main :: IO ()
main = do
  input <- map (read @Int) . lines <$> readFile "input/01"
  print . length . filter (uncurry (<)) . (zip <*> tail) $ input
  print . length . filter (uncurry (<)) . (zip <*> tail) . map (\(a, b, c) -> a + b + c) $ zip3 input (tail input) (drop 2 input)
