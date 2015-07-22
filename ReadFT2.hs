module ReadFT2 (
listOfWord32,
getl,
kv2string,
float2matrix,
spec2flists,
) where

import Data.Map as Map
import Data.Maybe (isJust,fromJust)
import Data.Word
import Data.Bits
import Data.Binary.Get
import GHC.ST (runST)
import qualified Data.ByteString.Lazy as BL
import Data.List.Split (chunksOf)
import Data.ReinterpretCast
import Numeric.LinearAlgebra

kv2string :: String -> Int -> Map Int String -> String  
kv2string error i xs
  | isJust $ Map.lookup i xs = fromJust $ Map.lookup i xs 
  | otherwise = error

-- 2048 hardcoded
spec2flists :: Int -> BL.ByteString -> [[Float]]
spec2flists i spec = chunksOf i $ Prelude.map wordToFloat (runGet listOfWord32 $ BL.drop 2048 spec)

float2matrix :: [[Float]] -> Matrix Float
float2matrix xs = fromLists xs :: Matrix Float

getBinary :: Get Word32
getBinary = do
  a <- getWord32be
  return a

listOfWord32 :: Get [Word32]
listOfWord32 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord32be
             rest <- listOfWord32
             return (v : rest)

getl :: [Word32] -> Int
getl sp = Prelude.length sp

-- main :: IO ()
-- main = do  
--   spec <-  BL.readFile "/home/jonas/Dropbox/nmrProg/development/src/prodecomp2/FT2_5D/s1010.ft2"
-- return $ getHeader spec
-- print $ B.length spec
-- sp <- runGet listOfWord32 spec
--  print $ getl $ runGet listOfWord32 $ BL.drop 2048 spec

