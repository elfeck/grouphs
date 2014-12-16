import Data.Maybe
import Data.Time

import Group
import ZnZGroup
import SnGroup

main :: IO ()
main = test1


-- Test Performance of tableToGroup type of groups
test1 = do
  let s4 = znz 100
  let s4tab = groupToTable s4
  let s4' = fromJust $ tableToGroup s4tab
  print $ order s4
  print $ order s4'
  start <- getCurrentTime
  print $ length (minimalGeneratingSets s4)
  stop <- getCurrentTime
  print (diffUTCTime stop start)
  print "==========="
  start <- getCurrentTime
  print $ length (minimalGeneratingSets s4')
  stop <- getCurrentTime
  print (diffUTCTime stop start)
