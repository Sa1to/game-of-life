{-# LANGUAGE ParallelListComp #-}
module Sum where
import Data.List
import Nested
import RightNeighbour

getSums :: [String] -> [[Int]]

getSums board = [zipWith (+)
                    (zipWith (+)
                        (zipWith (+) r l) (zipWith (+) t b)
                    )
                    (zipWith (+)
                        (zipWith (+) tr tl) (zipWith (+) br bl)
                     )
                | r <- rightNested board
                | l <- leftNested board
                | t <- topNested board
                | b <- botNested board
                | tr <- topRightNested board
                | tl <- topLeftNested board
                | br <- botRightNested board
                | bl <- botLeftNested board]
