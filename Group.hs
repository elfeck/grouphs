module Group (
  constructGroup,
  getMultTable,
  getOne,
  getInv
  ) where

import Data.List
import Data.Maybe
import GroupUtils

-- definitions
type BinOp a = (a -> a -> a)

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
  | not (null s) &&
    isJust (getOne_ s f) &&
    checkInv_ s f &&
    isAss_ s f = Just (Group s f)
  | otherwise = Nothing

getMultTable :: Eq a => Group a -> ([a], [[a]])
getMultTable (Group s f) = getMultTable_ s f

getOne :: Eq a =>  Group a -> a
getOne (Group s f) = fromJust (getOne_ s f)

getInv :: Eq a => Group a -> a -> a
getInv (Group s f) x = fromJust $ getInv_ s f x


-- private functions
checkInv_ :: (Eq a) => [a] -> BinOp a -> Bool
checkInv_ s f =
  length (filter isJust [getInv_ s f x | x <- s]) == length s

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

isAss_ :: Eq a => [a] -> BinOp a -> Bool
isAss_ s f = not $ False `elem` xs
  where xs = [f (f a b) c == f a (f b c) | a <- s, b <- s, c <- s]
