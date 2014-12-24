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
              isJust (one_ s f) &&
              checkInv s f &&
              checkClosed s f &&
              checkAssGen s f

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

one :: Eq a =>  Group a -> a
one (Group s f) = fromJust (one_ s f)

inv :: Eq a => Group a -> a -> a
inv (Group s f) x = fromJust $ inv_ s f x

order :: Eq a => Group a -> Int
order (Group s f) = length s

elemOrder :: Eq a => Group a -> a -> Int
elemOrder (Group s f) x = go (Group s f) x 1
  where go (Group s f) y acc
          | y == e = acc
          | otherwise = go (Group s f) (f y x) (acc + 1)
        e = one (Group s f)

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

minimalGenerator :: Eq a => Group a -> [a]
minimalGenerator g = head $ minimalGeneratingSets g

minimalGeneratingSets :: Eq a => Group a -> [[a]]
minimalGeneratingSets (Group s f) = go 1
  where go l | not (null (gener l)) = gener l
             | otherwise = go (l + 1)
        gener l = [x | x <- lenSubSets l s,
                   setEq s (generateFromSet (Group s f) x)]

generateFromSet :: Eq a => Group a -> [a] -> [a]
generateFromSet (Group s f) xs = go xs
  where go xs | setEq nextGen xs = xs
              | otherwise = go nextGen
          where nextGen = takeCrossProduct (Group s f)
                          (generateFromSetOnce (Group s f) xs)

-- not working
generateFromSet2 :: Eq a => Group a -> [a] -> [a]
generateFromSet2 (Group s f) xs = go xs
  where go xs | setEq nextGen xs = xs
              | otherwise = go nextGen
          where nextGen = takeCrossProduct (Group s f) xs

generateFromSetOnce :: Eq a => Group a -> [a] -> [a]
generateFromSetOnce (Group s f) xs =
  nub (foldl (++) [] [generateFrom (Group s f) x | x <- xs])

takeCrossProduct :: Eq a => Group a -> [a] -> [a]
takeCrossProduct (Group s f) xs = nub [f x y | x <- xs, y <- xs]

generateFrom :: Eq a => Group a -> a -> [a]
generateFrom (Group s f) x = go x x
  where go x y | y == e = [y]
               | otherwise = y : go x (f x y)
        e = one (Group s f)

subgroups :: Eq a => Group a -> [Group a]
subgroups g = go cyclics
  where go xs | length xs == length nextGen = xs
              | otherwise = go nextGen
          where nextGen = compoSubgroups g cyclics xs
        cyclics = cyclicSubgroups g

compoSubgroups :: Eq a => Group a -> [Group a] -> [Group a] -> [Group a]
compoSubgroups g ato comp = nubSubgroups r
  where r = map fromJust (filter isJust [subgrp a c | a <- ato, c <- comp])
        subgrp a c = constructGroup
                     (generateFromSet g (union (set a) (set c))) (op g)

cyclicSubgroups :: Eq a => Group a -> [Group a]
cyclicSubgroups (Group s f) = nubSubgroups subgrps
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
  length (filter isJust [inv_ s f x | x <- s]) == length s

checkSet :: Eq a => [a] -> Bool
checkSet s = not (null s) && s == nub s

checkClosed :: Eq a => [a] -> BinOp a -> Bool
checkClosed s f = setEq s (nub $ foldl (++) [] (snd $ groupToTable_ s f))

checkAss :: Eq a => [a] -> BinOp a -> Bool
checkAss s f = not $ False `elem` xs
  where xs = [f (f a b) c == f a (f b c) | a <- s, b <- s, c <- s]

checkAssGen :: Eq a => [a] -> BinOp a -> Bool
checkAssGen s f = checkAss (minimalGenerator (Group s f)) f

one_ :: Eq a => [a] -> BinOp a -> Maybe a
one_ s f | null xs = Nothing
            | otherwise = Just $ head xs
  where xs = [x | x <- s, f x someE == someE]
        someE = head s

one_2 :: Eq a => [a] -> BinOp a -> Maybe a
one_2 s f | isNothing ind = Nothing
             | otherwise = Just (s !! (fromJust ind))
  where ind = elemIndex s (snd (groupToTable_ s f))

inv_ :: Eq a => [a] -> BinOp a -> a -> Maybe a
inv_ s f x | isNothing e || not (x `elem` s) = Nothing
              | otherwise = find ((== fromJust e) . (f x)) s
  where e = one_ s f

groupToTable_ :: Eq a => [a] -> BinOp a -> MTable a
groupToTable_ s f = (s, [[f a b | a <- s] | b <- s])
