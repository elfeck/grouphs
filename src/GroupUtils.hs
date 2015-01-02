module GroupUtils where

import Data.List
import Data.Maybe

lenSubSets :: Eq a => Int -> [a] -> [[a]]
lenSubSets 0 _ = [[]]
lenSubSets _ [] = []
lenSubSets n (x : xs) = map (x :) (lenSubSets (n - 1) xs) ++ lenSubSets n xs

setEq :: Eq a => [a] -> [a] -> Bool
setEq s1 s2 = length s1 == length s2 && null (s1 \\ s2)

nubSEq :: Eq a => [[a]] -> [[a]]
nubSEq = nubBy setEq

tableToStringTable :: Show a => ([a], [[a]]) -> [[String]]
tableToStringTable (axis, tab) =
  ("f" : map show  axis) : [map show r | r <- fulltab]
  where fulltab = zipWith (\ax xs -> (ax : xs)) axis tab

tableToString :: Show a => ([a], [[a]]) -> Int -> String
tableToString (axis, tab) fp =
  frontPad ++ foldRow (head stab) ++ "\n\n" ++
  foldl (++) [] [frontPad ++ (foldRow row) ++ "\n" | row <- tail stab]
  where maxW = maximum $ map length (foldl (++) [] stab)
        stab = tableToStringTable (axis, tab)
        pad s n = s ++ replicate (n + maxW - length s) ' '
        foldRow row = pad (head row) 2 ++
                      foldl (++) [] [pad s 1 | s <- tail row]
        frontPad = replicate fp ' '

printMultTable :: Show a => ([a], [[a]]) -> IO()
printMultTable (axis, tab) = printS $ tableToString (axis, tab) 0

replaceChars :: String -> [Char] -> [Char] -> String
replaceChars [] _ _ = []
replaceChars (c : st) chars repl
  | c `elem` chars =
      repl !! (fromJust $ elemIndex c chars) : replaceChars st chars repl
  | otherwise = c : replaceChars st chars repl

printS :: String -> IO()
printS s = putStr s
