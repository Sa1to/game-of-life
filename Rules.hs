{-# LANGUAGE ParallelListComp #-}
module Rules where
import Dictionary


applyRuleForlLine :: String -> [Int] -> String
applyRuleForlLine b v = [applyBasicValidationRule c i | c <- b | i <- v]

applyBasicValidationRule :: Char -> Int -> Char
applyBasicValidationRule b v
                        | (b == toChar Dead) && (v == 3) = toChar Alive
                        | (b == toChar Alive) && (v < 2) = toChar Dead
                        | (b == toChar Alive) && (v > 3) = toChar Dead
                        | (b == toChar Alive) && ((v == 2) || (v == 3)) = toChar Alive
                        | otherwise = b
