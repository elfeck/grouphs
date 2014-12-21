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
    "G = {\n" ++ tableToString (groupToTable (Group s f)) 6 ++ "    }\n"

instance (Eq a) => Eq (Group a) where
  (==) g1 g2 = undefined


-- public functions
formsGroup :: Eq a => [a] -> BinOp a -> Bool
formsGroup s f = checkSet s &&
              isJust (getOne_ s f) &&
              checkInv s f &&
              checkClosed s f &&
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

elemOrder :: Eq a => Group a -> a -> Int
elemOrder (Group s f) x = go (Group s f) x 1
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
        go (x : xs) f | elemOrder (Group s f) x == o = True
                      | otherwise = go xs f
        o = order (Group s f)

minimalGeneratingSets :: Eq a => Group a -> [[a]]
minimalGeneratingSets (Group s f) = go 1
  where go l | not (null (gener l)) = gener l
             | otherwise = go (l + 1)
        gener l = [x | x <- lenSubSets l s,
                   setEq s (generateFromSet (Group s f) x)]

generateFromSet :: Eq a => Group a -> [a] -> [a]
generateFromSet (Group s f) xs = go xs
  where go gend | setEq (nextGen gend) gend = gend
                | otherwise = go (nextGen gend)
        nextGen ys = takeCrossProduct (Group s f)
                     (generateFromSetOnce (Group s f) ys)

generateFromSetOnce :: Eq a => Group a -> [a] -> [a]
generateFromSetOnce (Group s f) xs =
  nub (foldl (++) [] [generateFrom (Group s f) x | x <- xs])

takeCrossProduct :: Eq a => Group a -> [a] -> [a]
takeCrossProduct (Group s f) xs = nub ([f x y | x <- xs, y <- xs] ++
                                       [f y x | x <- xs, y <- xs])

generateFrom :: Eq a => Group a -> a -> [a]
generateFrom (Group s f) x = go x x
  where go x y | y == e = [y]
               | otherwise = y : go x (f x y)
        e = getOne (Group s f)

getSubgroups :: Eq a => Group a -> [Group a]
getSubgroups g = go cyclics
  where go xs | length xs == length nextGen = xs
              | otherwise = go nextGen
          where nextGen = getCompoSubgroups g cyclics xs
        cyclics = getCyclicSubgroups g

getCompoSubgroups :: Eq a => Group a -> [Group a] -> [Group a] -> [Group a]
getCompoSubgroups g ato comp = nubSubgroups r
  where r = map fromJust (filter isJust [subgrp a c | a <- ato, c <- comp])
        subgrp a c = constructGroup
                     (generateFromSet g (union (set a) (set c))) (op g)

getCyclicSubgroups :: Eq a => Group a -> [Group a]
getCyclicSubgroups (Group s f) = nubSubgroups subgrps
  where subgrps = map fromJust (
          filter isJust
          [constructGroup (generateFrom (Group s f) x) f | x <- s])

nubSubgroups :: Eq a => [Group a] -> [Group a]
nubSubgroups subgrps = go subgrps []
  where go [] ys = ys
        go (x : xs) ys
          | null [z | z <- ys, setEq (set x) (set z)] = go xs (x : ys)
          | otherwise = go xs ys

areIsomorphic :: (Eq a) => (Eq b) => Group a -> Group b -> Bool
areIsomorphic g1 g2
  | order g1 /= order g2 = False
  | not $ setEq (map (elemOrder g1) (set g1)) (map (elemOrder g2) (set g2)) =
      False
  | otherwise = True


-- functions for to-be groups
checkInv :: Eq a => [a] -> BinOp a -> Bool
checkInv s f =
  length (filter isJust [getInv_ s f x | x <- s]) == length s

checkSet :: Eq a => [a] -> Bool
checkSet s = not (null s) && s == nub s

checkClosed :: Eq a => [a] -> BinOp a -> Bool
checkClosed s f = setEq s (nub $ foldl (++) [] (snd $ groupToTable_ s f))

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
