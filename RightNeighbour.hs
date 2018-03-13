module RightNeighbour where
import Dictionary

rightList :: String -> [Int]

rightList (_:y:rest)
        | y == toChar Alive = 1 : rightList (y:rest)
        | otherwise = 0 : rightList (y:rest)

rightList _ = [0]
