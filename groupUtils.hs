module GroupUtils (
  multTableToStringTable,
  multTableToString,
  printMultTable
  ) where

import Data.List
import Data.Maybe

-- public functions
multTableToStringTable :: Show a => ([a], [[a]]) -> [[String]]
multTableToStringTable (axis, tab) =
  ["f"] : formatRow axis : [formatRow r | r <- fulltab]
  where fulltab = zipWith (\ax xs -> (ax : xs)) axis tab

multTableToString :: Show a => ([a], [[a]]) -> String
multTableToString = undefined

printMultTable :: Show a => ([a], [[a]]) -> IO()
printMultTable = undefined


-- private function
formatRow :: Show a => [a] -> [String]
formatRow row = undefined

replaceChars :: String -> [Char] -> [Char] -> String
replaceChars [] _ _ = []
replaceChars (c : st) chars repl
  | c `elem` chars =
      repl !! (fromJust $ elemIndex c chars) : replaceChars st chars repl
  | otherwise = c : replaceChars st chars repl

printS :: String -> IO()
printS s = putStr s
