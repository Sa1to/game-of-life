{-# LANGUAGE ParallelListComp #-}
module Sum
( getSums
) where
import Neighbour
import Control.Applicative

getSums :: [String] -> [[Int]]

getSums board = [getZipList $ (\a b c d e f g h -> a + b + c + d + e + f + g + h) <$>
                    ZipList r <*> ZipList l <*> ZipList t <*> ZipList b <*>
                    ZipList tr <*> ZipList tl <*> ZipList br <*> ZipList bl
                | r <- stateOfAllRightNeighbours board
                | l <- stateOfAllLeftNeighbours board
                | t <- stateOfAllTopNeighbours board
                | b <- stateOfAllBotNeighbours board
                | tr <- stateOfAllTopRightNeighbours board
                | tl <- stateOfAllTopLeftNeighbours board
                | br <- stateOfAllBotRightNeighbours board
                | bl <- stateOfAllBotLeftNeighbours board]
