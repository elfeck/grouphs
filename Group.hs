module Group where

import Data.List
import Data.Maybe
import GroupUtils

-- definitions
type BinOp a = (a -> a -> a)

type Cycle = [Int]
type Perm = [Cycle]

data Group a = Group {
  set :: [a],
  op :: (BinOp a)
  }

instance (Show a, Eq a) => Show (Group a) where
  show (Group s f) = "G = {\n" ++
                     multTableToString (getMultTable (Group s f)) 6 ++ "    }"


-- sample groups
znz :: Int -> Group Int
znz n = fromJust $ constructGroup [0..n-1] (\x y -> (x + y) `mod` n)


-- public functions
constructGroup :: Eq a => [a] -> BinOp a -> Maybe (Group a)
constructGroup s f
  | checkSet s &&
    isJust (getOne_ s f) &&
    checkInv s f &&
    isAss s f = Just (Group s f)
  | otherwise = Nothing

getMultTable :: Eq a => Group a -> ([a], [[a]])
getMultTable (Group s f) = getMultTable_ s f

getOne :: Eq a =>  Group a -> a
getOne (Group s f) = fromJust (getOne_ s f)

getInv :: Eq a => Group a -> a -> a
getInv (Group s f) x = fromJust $ getInv_ s f x

order :: Eq a => Group a -> Int
order (Group s f) = length s

orderElem :: Eq a => Group a -> a -> Int
orderElem (Group s f) x = go (Group s f) x e 1
  where e = getOne (Group s f)
        go (Group s f) x e acc
          | x == e = acc
          | f x x == e = acc + 1
          | otherwise = go (Group s f) (f x x) e (acc + 1)

isAbelian :: Eq a => Group a -> Bool
isAbelian g = tab == tab'
  where tab = snd (getMultTable g)
        tab' = [[l !! i | i <- [0..length (head tab) - 1]] | l <- tab]


-- helper for perm
applyPerm :: Perm -> Perm -> Perm
applyPerm = undefined

cycleToString :: Cycle -> String
cycleToString c = go (show c) where
  go [] = ""
  go (s : st) | s == ',' = go st
              | s == '[' = '(' : go st
              | s == ']' = ')' : go st
              | otherwise = s : go st

permToString :: Perm -> String
permToString cs = foldl (++) [] [cycleToString c | c <- cs]


-- functions to check group validity
checkInv :: Eq a => [a] -> BinOp a -> Bool
checkInv s f =
  length (filter isJust [getInv_ s f x | x <- s]) == length s

checkSet :: Eq a => [a] -> Bool
checkSet s = not (null s) && s == nub s

getOne_ :: Eq a => [a] -> BinOp a -> Maybe a
getOne_ s f | isNothing ind = Nothing
            | otherwise = Just (s !! (fromJust ind))
  where ind = elemIndex s (snd (getMultTable_ s f))

getMultTable_ :: Eq a => [a] -> BinOp a -> ([a], [[a]])
getMultTable_ s f = (s, [[f a b | a <- s] | b <- s])

getInv_ :: Eq a => [a] -> BinOp a -> a -> Maybe a
getInv_ s f x | isNothing e || not (x `elem` s) = Nothing
              | otherwise = find (((==) (fromJust e)) . (f x)) s
  where e = getOne_ s f

isAss :: Eq a => [a] -> BinOp a -> Bool
isAss s f = not $ False `elem` xs
  where xs = [f (f a b) c == f a (f b c) | a <- s, b <- s, c <- s]
