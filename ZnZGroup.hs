module ZnZGroup where

import Data.Maybe
import Group

znz :: Int -> Group Int
znz n = fromJust $ constructGroup [0..n-1] (\x y -> (x + y) `mod` n)
