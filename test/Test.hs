import Data.Maybe
import Data.Time
import Data.List

import Group
import ZnZGroup
import SnGroup
import GroupUtils

z5 = znz 5
z6 = znz 6
z7 = znz 7
z31 = znz 31
z57 = znz 57
z97 = znz 97

s3 = sn 3
s4 = sn 4
s5 = sn 5

main :: IO ()
main = test4

test4 = do
  print $ length $ subgroups2 s5

test3 = do
  let g = s5
  start <- getCurrentTime
  print $ length $ subgroups2 g
  end <- getCurrentTime
  print $ diffUTCTime end start


test1 = do
  let atoms = cyclicSubgroups s5
  let unions = nubBy setEq [union (set a) (set b) | a <- atoms, b <- atoms]
  print $ length unions
  putStr "===================\n"
  print $ length (catMaybes
         [constructGroup x (op s5) | x <- (map (generateFromSet s5) unions)])
  putStr "done\n"


test2 = do
  let g = s5
  let atoms = cyclicSubgroups g
  let unions = nubBy setEq [union (set a) (set b) | a <- atoms, b <- atoms]
  let gend = (map (generateFromSet g) unions)
  putStr "Start ################\n"
  start <- getCurrentTime
  print $ length gend
  --let ngend = nubBy setEq gend
  --print $ length ngend
  go 1 g gend
  end <- getCurrentTime
  print $ diffUTCTime end start

go _ _ [] = putStr "done"
go i g (x : xs) | order g `mod` length x == 0 = do
                    putStr ("#" ++ show i ++ " Constructing ... ")
                    start <- getCurrentTime
                    putStr (show (isJust (constructGroup
                                          (generateFromSet g x) (op g))))
                    end <- getCurrentTime
                    putStr $ " " ++ show (diffUTCTime end start) ++ "\n"
                    go (i + 1) g xs
                | otherwise = do
                    putStr ("#" ++ show i ++ " Discard \n")
                    go (i + 1) g xs
