module Nested where
import RightNeighbour
import Data.List
import Dictionary

rightNested :: [String] -> [[Int]]
rightNested = map rightList

leftNested :: [String] -> [[Int]]
leftNested = map ((reverse . rightList) . reverse)

botNested :: [String] -> [[Int]]
botNested lst = transpose $ rightNested $ transpose lst

topNested :: [String] -> [[Int]]
topNested lst = transpose $ leftNested $ transpose lst

botRightNested :: [String] -> [[Int]]
botRightNested lst = botNested [tail l ++ toString Dead | l <- lst]

topRightNested :: [String] -> [[Int]]
topRightNested lst = topNested [tail l ++ toString Dead | l <- lst]

botLeftNested :: [String] -> [[Int]]
botLeftNested lst = botNested [toString Dead ++ init l | l <- lst]

topLeftNested :: [String] -> [[Int]]
topLeftNested lst = topNested [toString Dead ++ init l | l <- lst]


