module Group where

import Data.List
import Data.Maybe

import GroupUtils

-- definitions
type BinOp a = (a -> a -> a)
type MTable a = ([a], [[a]])

data Group a = Group { set :: [a], op :: (BinOp a) }

instance (Show a, Eq a) => Show (Group a) where
  show (Group s f) =
    "G = {\n" ++ tableToString (groupToTable (Group s f)) 6 ++ "    }"


-- public functions
formsGroup :: Eq a => [a] -> BinOp a -> Bool
formsGroup s f = checkSet s &&
              isJust (getOne_ s f) &&
              checkInv s f &&
              checkAss s f

isValidGroup :: Eq a => Group a -> Bool
isValidGroup (Group s f) = formsGroup s f

constructGroup :: Eq a => [a] -> BinOp a -> Maybe (Group a)
constructGroup s f | formsGroup s f = Just (Group s f)
                   | otherwise = Nothing

tableToGroup :: Eq a => MTable a -> Maybe (Group a)
tableToGroup (axis, tab) = constructGroup axis (tableToFunction (axis, tab))

tableToFunction :: Eq a => MTable a -> BinOp a
tableToFunction (axis, tab)  = \x y -> (tab !! pos x) !! pos y
  where pos x = fromJust $ elemIndex x axis

groupToTable :: Eq a => Group a -> MTable a
groupToTable (Group s f) = groupToTable_ s f

getOne :: Eq a =>  Group a -> a
getOne (Group s f) = fromJust (getOne_ s f)

getInv :: Eq a => Group a -> a -> a
getInv (Group s f) x = fromJust $ getInv_ s f x

order :: Eq a => Group a -> Int
order (Group s f) = length s

orderElem :: Eq a => Group a -> a -> Int
orderElem (Group s f) x = go (Group s f) x 1
  where go (Group s f) y acc
          | y == e = acc
          | otherwise = go (Group s f) (f y x) (acc + 1)
        e = getOne (Group s f)

isAbelian :: Eq a => Group a -> Bool
isAbelian g = tab == tab'
  where tab = snd (groupToTable g)
        tab' = [[l !! i |  l <- tab] | i <- [0..length (head tab) - 1]]

isCyclic :: Eq a => Group a -> Bool
isCyclic (Group s f) = go s f
  where go [] f = False
        go (x : xs) f | orderElem (Group s f) x == o = True
                      | otherwise = go xs f
        o = order (Group s f)

minimalGeneratorSet :: Eq a => Group a -> [a]
minimalGeneratorSet (Group s f) = undefined

generateFrom :: Eq a => Group a -> a -> [a]
generateFrom (Group s f) x = go x x
  where go x y | y == e = [y]
               | otherwise = y : go x (f x y)
        e = getOne (Group s f)

lenSubSets :: Eq a => Int -> [a] -> [[a]]
lenSubSets 0 _ = [[]]
lenSubSets _ [] = []
lenSubSets n (x : xs) = map (x :) (lenSubSets (n - 1) xs) ++ lenSubSets n xs

-- functions for to-be groups
checkInv :: Eq a => [a] -> BinOp a -> Bool
checkInv s f =
  length (filter isJust [getInv_ s f x | x <- s]) == length s

checkSet :: Eq a => [a] -> Bool
checkSet s = not (null s) && s == nub s

checkAss :: Eq a => [a] -> BinOp a -> Bool
checkAss s f = not $ False `elem` xs
  where xs = [f (f a b) c == f a (f b c) | a <- s, b <- s, c <- s]

getOne_ :: Eq a => [a] -> BinOp a -> Maybe a
getOne_ s f | null xs = Nothing
             | otherwise = Just $ head xs
  where xs = [x | x <- s, f x someE == someE]
        someE = head s

getOne_2 :: Eq a => [a] -> BinOp a -> Maybe a
getOne_2 s f | isNothing ind = Nothing
            | otherwise = Just (s !! (fromJust ind))
  where ind = elemIndex s (snd (groupToTable_ s f))

getInv_ :: Eq a => [a] -> BinOp a -> a -> Maybe a
getInv_ s f x | isNothing e || not (x `elem` s) = Nothing
              | otherwise = find ((== fromJust e) . (f x)) s
  where e = getOne_ s f

groupToTable_ :: Eq a => [a] -> BinOp a -> MTable a
groupToTable_ s f = (s, [[f a b | a <- s] | b <- s])
