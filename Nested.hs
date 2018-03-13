module Nested where
import RightNeighbour
import Data.List


rightNested :: [String] -> [[Int]]
rightNested = map rightList

leftNested :: [String] -> [[Int]]
leftNested = map ((reverse . rightList) . reverse)

botNested :: [String] -> [[Int]]
botNested lst = transpose $ rightNested $ transpose lst

topNested :: [String] -> [[Int]]
topNested lst = transpose $ leftNested $ transpose lst

botRightNested :: [String] -> [[Int]]
botRightNested lst = botNested [tail l ++ "0" | l <- lst]

topRightNested :: [String] -> [[Int]]
topRightNested lst = topNested [tail l ++ "0" | l <- lst]

botLeftNested :: [String] -> [[Int]]
botLeftNested lst = botNested ["0" ++ init l | l <- lst]

topLeftNested :: [String] -> [[Int]]
topLeftNested lst = topNested ["0" ++ init l | l <- lst]


