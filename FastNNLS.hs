-- implementation of fastNNLS, a first try
-- original code from Bro, implemented in matlab
module FastNNLS (
  listv,
  listm)
where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.List hiding (find)
import Data.Maybe
import Control.Monad
import Control.Monad.Writer

-- functions:
 
getMaxValue :: [Int] -> Vector Double -> Double
getMaxValue xs v = maximum (listv xs (toList v)) 

getMaxValueM :: [Int] -> Vector Double -> Maybe Double
getMaxValueM xs v 
    | (length xs) <= (size v) = Just $ maximum (listv xs (toList v))
    | otherwise = Nothing

getMaxIndex :: [Int] -> Vector Float -> Int
getMaxIndex xs v = maxIndex $ fromList (listv xs (toList v)) 

getMaxIndexM :: [Int] -> Vector Double -> Maybe Int
getMaxIndexM xs v = Just $ maxIndex $ fromList (listv xs (toList v)) 

--  t <- Just y >>= (\x -> if (x == 0) then fail "index error" else Just(x + 1))
--maxInd :: [a] -> Maybe Int
--maxInd xs = elemIndex (maximum xs) xs

-- test1 :: Int -> Maybe Int
-- test1 y = do
--   s <- Just y >>= (\x -> if (x == 0) then fail "zero" else Just(x + 1))
--   let s = test2 10
--   s <- Just (s+100) >>= (\x -> if (x == 0) then fail "zero" else Just(x + 1))
--   return s

getValueM :: [a] -> Int -> Maybe a
getValueM xs y 
    | (length xs) < y = Nothing
    | otherwise = Just $ xs !! y

getIndexesM :: Matrix Double -> [Int] -> Matrix Double
getIndexesM xtx pp = (xtx ? pp) ¿ pp  

absV :: Vector Double -> Vector Double
absV v = cmap abs v

findl :: Int -> [Int] -> [Int]
findl p xs = [x | x <- [0..(length xs)-1], (xs !! x)/=p]

andL :: [Int] -> [Int] -> [Int]
andL v1 v2 = zipWith (\x y -> if (x>0 && y>0) then 1 else 0) v1 v2

andV :: Vector Double -> Vector Int -> Vector Double
andV v1 v2 = zipVectorWith (\x y -> if (x>0 && y>0) then 1 else 0) v1 v2

tolv :: Vector Double -> Double -> Vector Double
tolv v tol = cmap (\x -> if x >= tol then 1 else 0) v

toll :: [Double] -> Double -> [Double]
toll v tol = map (\x -> if x >= tol then x else 0) v

getIndexesV :: Vector Double -> [Int] -> Vector Double
getIndexesV xty pp = fromList $ filter (>0) $ map (\x -> xty ! x) pp  

getIndxV :: Vector Double -> [Int] -> Vector Double
getIndxV xty pp = fromList (map (\x -> xty ! x) pp)

getIndexesL :: [Int] -> [Int] -> [Int]
getIndexesL z pp = filter (>0) $ map (\x -> z !! x) pp  

getIndexes2L :: [Int] -> [Int] -> [Int]
getIndexes2L z pp = map (\x -> z !! x) pp

zerosL :: [Int] -> [Int] -> [Int]
zerosL p ij = zipWith (\x y -> if elem y ij then 0 else x) p [0..(length p)-1]
             
-- zerosV :: Vector Double ->  [Int] ->  Vector Double
-- zerosV z zz = zipVectorWith (\x y -> if elem y zz then 0 else x) z (vector [0..(size z)-1])

replL :: [Int] -> [Int] -> [Int]
replL p ij = zipWith (\x y -> if elem y ij then y else x) p [0..(length p)-1]

replL2 :: [Int] -> Int -> Int -> [Int]
replL2 p i v = zipWith (\x y -> if y == i then v else x) p [0..(length p)-1]

replaceValueL :: [Int] -> Int -> Int -> Maybe [Int]
replaceValueL xs t a
    | t<(length xs) = Just $ map (\x -> if x/=t then x else a) xs
    | otherwise = Nothing

replaceValue2L :: [Int] -> Int -> Int -> Maybe [Int]
replaceValue2L xs t a
    | t<(length xs) = Just $ map (\x -> if x/=t then x else a) xs
    | otherwise = Nothing

listv :: [Int] -> [a] -> [a]
listv [] _ = []
listv (x:xp) sp = sp !! x : listv xp sp 

listm :: [Int] -> [[a]] -> [a]
listm xs sp = listv xs (concat sp) 

-- initializing

xtx = matrix 5 [1..25] -- temporary solution
--xtx = uniformSample 1 5 [(0,1),(0,1),(0,1),(0,1),(0,1)]

--xty =  randomVector 1 Uniform 5
xty = vector [1..5] -- temporary solution

m :: Int
m = (fst (size xtx)) -- [m,n] = size(XtX)

n :: Int
n = (snd (size xtx)) -- [m,n] = size(XtX)

tol :: Double
tol = 10 * Numeric.LinearAlgebra.eps -- * (norm1 xtx) * (max (size xtx))  -- norm(XtX,1)*max(size(XtX))

p :: [Int]
p = [0 | x <- [1..n]] -- P = zeros(1,n)
--p = konst 0 n :: Vector Double

x :: Vector Double
-- x = randomVector 1 Uniform n
x = vector [0 | x <- [1..n]] -- x = P'

nzz :: Int
nzz = 100

iter :: Int
iter = 0 

itmax :: Int
itmax = 30 * n

z :: [Int] -- Z = 1:n;
z = [0..n-1]

zz :: [Int]
zz = z -- ZZ=Z

w :: Vector Double
w = x - app xtx xty -- w = Xty-XtX*x

-- while any(Z) & any(w(ZZ) > tol): loops until Z has zero entries > 0 and for all index of zz where w is bigger than zero
    
-- matlab:

    -- [wt,t] = max(w(ZZ)); # wt=max value and t=index
    -- t = ZZ(t);
    -- P(1,t) = t;
    -- Z(t) = 0;
    -- PP = find(P);
    -- ZZ = find(Z);
    -- nzz = size(ZZ);
    -- z(PP')=(Xty(PP)'/XtX(PP,PP)');
    -- z(ZZ) = zeros(nzz(2),nzz(1))';
    -- z=z(:);

        -- iter = iter + 1;
        -- QQ = find((z <= tol) & P');
        -- alpha = min(x(QQ)./(x(QQ) - z(QQ)));
        -- x = x + alpha*(z - x);
        -- ij = find(abs(x) < tol & P' ~= 0);
        -- Z(ij)=ij';
        -- P(ij)=zeros(1,length(ij));
        -- PP = find(P);
        -- ZZ = find(Z);
        -- nzz = size(ZZ);
        -- z(PP)=(Xty(PP)'/XtX(PP,PP)');
        -- z(ZZ) = zeros(nzz(2),nzz(1));
        -- z=z(:);

flog :: Maybe Double -> Maybe Double
flog x = x

outerLoop :: Matrix Double -> Vector Double -> Maybe Double
--outerLoop :: Matrix Double -> Vector Double -> Maybe (Double,Int,[Int],[Int],[Int],[Int],Int,Matrix Double,Vector Double,Vector Double,[Int])
outerLoop xtx xty = do
  wt <- getMaxValueM zz w       -- [wt,t] = max(w(ZZ))
  show flog $ Just wt
  t <- getMaxIndexM zz w        -- [wt,t] = max(w(ZZ))
  t <- getValueM zz t           -- t = ZZ(t);
  p <- Just $ replL2 p t t             -- P(1,t) = t;
  z <- Just $ replL2 z t 0             -- Z(t) = 0;
  pp <- Just $ findl 0 p        -- PP = find(P);
  zz <- Just $ findl 0 z        -- ZZ = find(Z);
  nzz <- Just $ length zz       -- nzz = size(ZZ);
  xtxp <- Just $ getIndexesM xtx pp -- XtX(PP,PP)
  xtyp <- Just $ getIndexesV xty pp  -- Xty(PP)
  zzz <- Just $ xtx <\> xty        -- Xty(PP)/XtX(PP,PP)
  z <- Just $ zerosL z zz       -- z(ZZ) = zeros(nzz(2),nzz(1))'
  alpha <- innerLoop x p z zzz tol iter
  return alpha

innerLoop :: Vector Double -> [Int] -> [Int] -> Vector Double -> Double -> Int -> Maybe Double
innerLoop x p z zzz tol iter = do
  qq <- Just $ find (>0) (andV (tolv x tol) $ fromList p)       -- QQ = find((z <= tol) & P')
  let alpha = minElement ((getIndxV x qq) / ((getIndxV x qq) - (getIndxV zzz qq))) -- min(x(QQ)./(x(QQ) - z(QQ)))
  x <- Just $ (x + (scale alpha (zzz-x)))                         -- x = x + alpha*(z - x)
  ij <- Just $ find (>0) (andV (tolv (abs x) tol) (fromList p)) -- ij = find(abs(x) < tol & P' ~= 0)
  z <- Just $ replL z ij                                        -- Z(ij)=ij'
  p <- Just $ zerosL p ij                                       -- P(ij)=zeros(1,length(ij))
  pp <- Just $ filter (>0) p                                    -- PP = find(P);
  zz <- Just $ filter (>0) z                                    -- ZZ = find(Z);
  nzz <- Just $ length zz                                       -- nzz = size(ZZ);
  xtxp <- Just $ getIndexesM xtx pp                              -- XtX(PP,PP)
  xtyp <- Just $ getIndexesV xty pp                              -- Xty(PP)
  zzz <- Just $ xtx <\> xty                                     -- Xty(PP)/XtX(PP,PP)
  z <- Just $ zerosL z zz                                       -- z(ZZ) = zeros(nzz(2),nzz(1))
  return alpha

-- while :: (Monad m) => m Bool -> m a -> m ()
-- while cond action = do
--     c <- cond
--     when c $ do
--         action
--         while cond action

-- fastnnls :: Matrix Double -> Vector Double -> Double
-- fastnnls xtx xty = do
--   zzz <- outerLoop xtx xty
-- --  alpha <- innerLoop x p z zzz tol iter
--   if any (>0) z then outerLoop xtx xty else return x -- while any(Z) & any(w(ZZ) > tol)
--   -- alpha <- innerLoop x p zzz tol iter
--   -- if alpha > 1 then innerLoop x p zzz tol iter else outerLoop xtx xty  

--   x = z;
--   w = Xty-XtX*x;
