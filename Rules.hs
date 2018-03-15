{-# LANGUAGE ParallelListComp #-}
module Rules where
import Dictionary


applyRuleForlLine :: String -> [Int] -> String
applyRuleForlLine boardChar value = [applyBasicValidationRule c i | c <- boardChar | i <- value]

applyBasicValidationRule :: Char -> Int -> Char
applyBasicValidationRule boardChar value
                        | (boardChar == toChar Dead) && (value == 3) = toChar Alive
                        | (boardChar == toChar Alive) && (value < 2) = toChar Dead
                        | (boardChar == toChar Alive) && (value > 3) = toChar Dead
                        | (boardChar == toChar Alive) && ((value == 2) || (value == 3)) = toChar Alive
                        | otherwise = boardChar
