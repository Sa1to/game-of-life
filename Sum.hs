{-# LANGUAGE ParallelListComp #-}
module Sum where
import Data.List
import Neighbour

getSums :: [String] -> [[Int]]

getSums board = [zipWith (+)
                    (zipWith (+)
                        (zipWith (+) r l) (zipWith (+) t b)
                    )
                    (zipWith (+)
                        (zipWith (+) tr tl) (zipWith (+) br bl)
                     )
                | r <- stateOfAllRightNeighbours board
                | l <- stateOfAllLeftNeighbours board
                | t <- stateOfAllTopNeighbours board
                | b <- stateOfAllBotNeighbours board
                | tr <- stateOfAllTopRightNeighbours board
                | tl <- stateOfAllTopLeftNeighbours board
                | br <- stateOfAllBotRightNeighbours board
                | bl <- stateOfAllBotLeftNeighbours board]
