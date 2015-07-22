module ReadProdecompTxt (
getParams,
getParamx,
getPath,
getPath2,
getDefinitions,
toInt,
toFloat)
where

import Data.List.Split
--import qualified Data.Map as Map
import Data.Typeable

matchString :: String -> String -> Bool
matchString a x = elem a $ splitOn " " x

removeC :: [String] -> [String]
removeC xs = map (\x -> if (last x)==',' then init x else x) xs

removeP :: [String] -> [String]
removeP xs = map (\x -> if (head x)=='(' && (last x)==')' then init $ tail x else x) xs

toFloat :: [String] -> [Float]
toFloat xs = map read xs :: [Float]

toInt :: [String] -> [Int]
toInt xs = map read xs :: [Int]

getPath :: String -> Int -> [String] -> [(Int,String)]
getPath _ _ [] = []
getPath a i (x:xs)
    | matchString a x = zip [i] [splitOn " " x !! 2] ++ getPath a (i+1) xs
    | otherwise = getPath a i xs

getPath2 :: String -> [String] -> [String]
getPath2 _ [] = []
getPath2 a (x:xs)
    | matchString a x = [splitOn " " x !! 2] ++ getPath2 a xs
    | otherwise = getPath2 a xs

printString :: [String] -> [String]
printString xs = xs

getParams :: String -> [String] -> [(Int,String)]  
getParams _ [] = []
getParams a (x:xs) 
  | matchString a x = zip [1..] (removeP $ removeC $ tail $ splitOn " " x)
  | otherwise = getParams a xs

getParamx :: ([String] -> [a]) -> String -> [String] -> [(Int,a)]  
getParamx _ _ [] = []
getParamx f a (x:xs) 
  | matchString a x = zip [1..] (f $ removeP $ removeC $ tail $ splitOn " " x)
  | otherwise = getParamx f a xs

getDefinitions :: String -> [String] -> [[String]]  
getDefinitions _  [] = [] 
getDefinitions a (x:xs) 
  | matchString a x = [filter (\d -> if d=="1" || d=="-1" || d=="0" then True else False) $ splitOneOf ", " x] ++ getDefinitions a xs
  | otherwise = getDefinitions a xs
