{-# LANGUAGE TypeApplications #-}

module Day03 (main) where

import Control.Lens
import Data.Bits (complement)
import Data.Char (digitToInt)
import Data.Digits (unDigits)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (partition)
import Data.List.Extra (minimumOn)
import Data.Ratio ((%))
import Relude (transpose, (>>>))

main :: IO ()
main =
  do
    input <- (map . map) digitToInt . lines <$> readFile "input/03" :: IO [[Int]]
    input
      & transpose
      & map (head . minimumOn length . toListOf each . partition (== 1))
      & ((*) `on` unDigits 2) <*> map ((`mod` 2) . (+ 1))
      & print
