module Decompose (
selectSpectra,
isConvolution,
decompose,
getSlice
) where

import Data.Map as Map
import Numeric.LinearAlgebra as NL

-- decomposition of projection experiments

selectSpectra :: Int -> Map Int [Int] -> Map Int [Int]
selectSpectra i xs = Map.filterWithKey (\_ v -> v !! i /= 0) xs

isConvolution :: Int -> [Int] -> Bool
isConvolution _ [] = False
isConvolution s (y:x:xs)  
  | x /= 0 && x /= s = True
  | otherwise = isConvolution s xs

getSlice :: Int -> Matrix Float -> Vector Float 
getSlice x spec = (NL.toColumns spec) !! x 

-- getSpectra :: Int -> Map Int (Matrix Float) -> Matrix Float   
-- getSpectra x sp = sp (Map.!) x

-- getAllSlices :: [Int] -> Vector Float
-- getAllSlices xs = map getSlice ...

-- permutate :: Int -> [a] -> [a]
-- permutate _ [] = []
-- permutate n xs = zipWith const (drop n (cycle xs)) xs

decompose :: Map Int [String] -> Int
decompose xs = 0
