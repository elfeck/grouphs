import Data.Maybe
import Data.Time
import Data.List

import Group
import ZnZGroup
import SnGroup

z5 = znz 5
z6 = znz 6
z7 = znz 7
z31 = znz 31
z57 = znz 57
z97 = znz 97

s3 = sn 3
s4 = sn 4

main :: IO ()
main = test4

test4 = do
  print $ subgroups $ znz (2 * 3 * 7 * 11)

test3 = do
  let zn500 = znz 500
  print $ order zn500

test1 = do
  let s4subs = subgroups s4
  print $ length s4subs
  print $ sort (map order s4subs)

test2 = do
  let s5 = sn 5
  start <- getCurrentTime
  print $ order s5
  end <- getCurrentTime
  print $ diffUTCTime end start
  putStr "\n# normal ass\n"
  start <- getCurrentTime
  print $ checkAss (set s5) (op s5)
  end <- getCurrentTime
  print $ diffUTCTime end start
  putStr "\n"
  putStr "# gen ass\n"
  start <- getCurrentTime
  print $ checkAssGen (set s5) (op s5)
  end <- getCurrentTime
  print $ diffUTCTime end start
