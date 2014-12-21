module ZnZGroup where

import Data.Maybe
import Data.List
import Group

znz :: Int -> Group Int
znz n = fromJust $ constructGroup [0..n-1] (\x y -> (x + y) `mod` n)

znzx :: Int -> Group Int
znzx 1 = fromJust $ constructGroup [1] (*)
znzx n = fromJust $ constructGroup (allCoprimes n)
         (\x y -> (x * y) `mod` n)

allCoprimes :: Int -> [Int]
allCoprimes n = [x | x <- [1..n], isCoprime x n]

isCoprime :: Int -> Int -> Bool
isCoprime 1 _ = True
isCoprime _ 1 = True
isCoprime a b = null $ intersect (primFactors a) (primFactors b)

primFactors :: Int -> [Int]
primFactors n
  | not (null factors) = (head factors) : primFactors (n `div` (head factors))
  | otherwise = [n]
  where factors = [x | x <- [2..n - 1], n `mod` x == 0]
