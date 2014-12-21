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
main = test

test = do
  let s4subs = getSubgroups s4
  print $ length s4subs
  print $ sort (map order s4subs)
