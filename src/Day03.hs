module Day03 (main) where

import Control.Lens
import Data.Char (digitToInt)
import Data.Digits (unDigits)
import Data.Function (on)
import Debug.Trace (trace)
import Relude (traceShowId, transpose, (>>>))

main :: IO ()
main = do
  input <- readFile "input/03" <&> (lines >>> each . each %~ digitToInt)
  print . (on (*) (unDigits 2) <*> map (\x -> mod (x + 1) 2)) . map (selectBitFromSign (>= 0)) . transpose $ input
  let o2 = unDigits 2 $ rating (>= 0) input
      co2 = unDigits 2 $ rating (< 0) input
  print (o2 * co2)

rating :: (Int -> Bool) -> [[Int]] -> [Int]
rating _ [] = []
rating _ [rest] = rest
rating p candidates = selected : rating p newCandidates
  where
    selected = selectBitFromSign p . head . transpose $ candidates
    newCandidates = map tail . filter ((selected ==) . head) $ candidates

selectBitFromSign :: (Int -> Bool) -> [Int] -> Int
selectBitFromSign p = fromEnum . p . sum . map (\x -> x * 2 - 1)
