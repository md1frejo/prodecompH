--module Prodecomp3 where

import Constants
import ReadProdecompTxt
import ReadFT2
import Decompose
import FastNNLS
import Midmatrix

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Numeric.LinearAlgebra
import Data.ReinterpretCast
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import System.Random

test :: Float -> Float
test a = a

main :: IO ()
main = do
  prodtxt <- readFile "prodecomp.txt"
  let paths =  (Map.fromList . zip [1..] . getPath2 "PATH:" . lines) prodtxt
  let paths2 = getPath2 "PATH:" $ lines prodtxt
  let nuclei = (Map.fromList . getParams "NUCLEI=" . lines) prodtxt
  let swppm = (Map.fromList . getParamx toFloat "SW_ppm=" . lines) prodtxt
  let swhz = (Map.fromList . getParamx toFloat "SW_hz=" . lines) prodtxt
  let offs = (Map.fromList . getParamx toFloat "O1_ppm=" . lines) prodtxt
  let size = (Map.fromList . getParamx toInt "SIZE=" . lines) prodtxt
  let dirp = fromJust $ Map.lookup 1 size
  let indp = fromJust $ Map.lookup 2 size
  let defs = (Map.fromList . zip [1..] . map toInt . getDefinitions "DEFINITION:" . lines) prodtxt
  temp <- mapM BL.readFile paths2
  let spectra = Map.fromList $ zip [1..] (map float2matrix (map (spec2flists indp) temp))
  let selected = selectSpectra 2 defs
--  let rand = test randomIO :: IO Float
--  let slice = join $ map getSlice $ Map.keys selected 
  putStrLn "calling Prodecomp3 main"
  putStrLn "paths:"        
  print paths
  putStrLn "nuclei:"
  print nuclei
  putStrLn "swppm:"
  print swppm
  putStrLn "swhz:"
  print swhz
  putStrLn "offs:"
  print offs
  putStrLn "size:"
  print size
  putStrLn "defs:"
  print defs
  print $ Map.size spectra
  print selected
  putStrLn "rand:"
--  print $ test randomIO :: IO Float
  
--  print $ length slice
--  print $ spectra (Map.!) 1

