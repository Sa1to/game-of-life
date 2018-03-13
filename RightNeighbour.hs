module RightNeighbour where

rightList :: String -> [Int]

rightList (_:y:rest)
        | y == '*' = 1 : rightList (y:rest)
        | otherwise = 0 : rightList (y:rest)

rightList _ = [0]
