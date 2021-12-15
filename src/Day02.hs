module Day02 (
  main,
) where

import Control.Lens
import Relude ((>>>))

main :: IO ()
main = do
  input <- map (words >>> (\[s, i] -> (s, read i :: Int))) . lines <$> readFile "input/02"
  -- print . uncurry (*) . foldl step (0, 0) $ input
  let (x, y, _) = foldl step' (0, 0, 0) $ input
  print (x * y)

step (x, y) ("forward", i) = (x + i, y)
step (x, y) ("down", i) = (x, y + i)
step (x, y) ("up", i) = (x, y - i)

step' (x, y, aim) ("forward", i) = (x + i, y + i * aim, aim)
step' (x, y, aim) ("down", i) = (x, y, aim + i)
step' (x, y, aim) ("up", i) = (x, y, aim - i)
