module GroupUtils (
  tableToStringTable,
  tableToString,
  printMultTable
  ) where

import Data.List
import Data.Maybe

-- public functions
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


-- private function
replaceChars :: String -> [Char] -> [Char] -> String
replaceChars [] _ _ = []
replaceChars (c : st) chars repl
  | c `elem` chars =
      repl !! (fromJust $ elemIndex c chars) : replaceChars st chars repl
  | otherwise = c : replaceChars st chars repl

printS :: String -> IO()
printS s = putStr s