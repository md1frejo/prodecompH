module Midmatrix (
  midmatrix,
  rotate,
  permute2
  ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

permute2 :: Int -> [a] -> [[a]]
permute2 0 _ = []
permute2 y xs = [rotate y xs] ++ permute2 (y-1) xs   

midmatrix :: Int -> Int -> Vector Float -> Matrix Float
midmatrix sign ind v  
          | sign == -1 = fromLists (permute2 ind (reverse (toList v))) 
          | otherwise = fromLists (permute2 ind (toList v)) 
