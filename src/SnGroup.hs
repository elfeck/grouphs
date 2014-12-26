module SnGroup where

import Data.List
import Data.Maybe

import Group


{-
  Types
-}
type Cycle = [Int]
type Perm = [Cycle]

newtype Permuta = Permuta Perm

instance Show Permuta where
  show (Permuta p) = permToString p

instance Eq Permuta where
  (==) (Permuta p1) (Permuta p2) = permEqual p1 p2

{-
  Sn
-}
sn :: Int -> Group Permuta
sn n = fromJust $ constructGroup (map Permuta (constructPerms n)) applyPermuta

applyPermuta :: Permuta -> Permuta -> Permuta
applyPermuta (Permuta p1) (Permuta p2) = Permuta (applyPerm p1 p2)


{-
  Helper
-}
constructPerms :: Int -> [Perm]
constructPerms n =
  [permFromFunction n (f perm) | perm <- permutations [1..n]]
  where f p = (\x -> p !! (x - 1))

applyPerm :: Perm -> Perm -> Perm
applyPerm a b = permFromFunction (permMax a) $
                (permToFunction a) . (permToFunction b)

permFromFunction :: Int -> (Int -> Int) -> Perm
permFromFunction n p = nubCycles [cycleFromElem n x p | x <- [1..n]]

nubCycles :: [Cycle] -> Perm
nubCycles cs = go cs []
  where go [] acc = acc
        go (c : p) acc | null (filter (cycleEqual c) acc) = go p (acc ++ [c])
                       | otherwise = go p acc

cycleFromElem :: Int -> Int -> (Int -> Int) -> Cycle
cycleFromElem n x p = go x [x] p
  where go x cy p | (p x) `elem` cy = cy
                  | otherwise = go (p x) (cy ++ [p x]) p

-- only applicable for well defined permutations
permToFunction :: Perm -> (Int -> Int)
permToFunction p = \x -> cycleToFunction (getCycle x) x
  where getCycle x = head [y | y <- p, x `elem` y]

-- gives function _only_ defined for elements in cs
cycleToFunction :: Cycle -> (Int -> Int)
cycleToFunction cs = \x -> rotCs !! (fromJust $ elemIndex x cs)
  where rotCs = tail cs ++ [head cs]

permContains :: Perm -> Int -> Bool
permContains xs x = x `elem` foldl (++) [] xs

permMax :: Perm -> Int
permMax p = maximum $ foldl (++) [] p

cycleEqual :: Cycle -> Cycle -> Bool
cycleEqual c1 c2
  | sort c1 /= sort c2 = False
  | otherwise = null [x | x <- c1,
                      cycleToFunction c1 x /= cycleToFunction c2 x]

permEqual :: Perm -> Perm -> Bool
permEqual p1 p2 = null [x | x <- [1..permMax p1],
                        (permToFunction p1 x) /= (permToFunction p2 x)]

permToString :: Perm -> String
permToString cs = foldl (++) [] [cycleToString c | c <- cs]

cycleToString :: Cycle -> String
cycleToString c = go (show c) where
  go [] = ""
  go (s : st) | s == ',' = go st
              | s == '[' = '(' : go st
              | s == ']' = ')' : go st
              | otherwise = s : go st
