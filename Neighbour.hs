module Neighbour where
import Data.List
import Dictionary

stateOfRightNeighbour :: String -> [Int]

stateOfRightNeighbour (_:y:rest)
        | y == toChar Alive = 1 : stateOfRightNeighbour (y:rest)
        | otherwise = 0 : stateOfRightNeighbour (y:rest)
stateOfRightNeighbour _ = [0]

stateOfAllRightNeighbours :: [String] -> [[Int]]
stateOfAllRightNeighbours = map stateOfRightNeighbour

stateOfAllLeftNeighbours :: [String] -> [[Int]]
stateOfAllLeftNeighbours = map ((reverse . stateOfRightNeighbour) . reverse)

stateOfAllBotNeighbours :: [String] -> [[Int]]
stateOfAllBotNeighbours lst = transpose $ stateOfAllRightNeighbours $ transpose lst

stateOfAllTopNeighbours :: [String] -> [[Int]]
stateOfAllTopNeighbours lst = transpose $ stateOfAllLeftNeighbours $ transpose lst

stateOfAllBotRightNeighbours :: [String] -> [[Int]]
stateOfAllBotRightNeighbours lst = stateOfAllBotNeighbours [tail l ++ toString Dead | l <- lst]

stateOfAllTopRightNeighbours :: [String] -> [[Int]]
stateOfAllTopRightNeighbours lst = stateOfAllTopNeighbours [tail l ++ toString Dead | l <- lst]

stateOfAllBotLeftNeighbours :: [String] -> [[Int]]
stateOfAllBotLeftNeighbours lst = stateOfAllBotNeighbours [toString Dead ++ init l | l <- lst]

stateOfAllTopLeftNeighbours :: [String] -> [[Int]]
stateOfAllTopLeftNeighbours lst = stateOfAllTopNeighbours [toString Dead ++ init l | l <- lst]


