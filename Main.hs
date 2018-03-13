{-# LANGUAGE ParallelListComp #-}
module Main where
import System.IO
import Control.Monad
import Data.List 
import System.Console.ANSI
import Sum
import Dictionary

main = do
    clearScreen
    file <- openFile "./resources/board.txt" ReadMode
    content <- hGetContents file
    let l = lines content
    mapM_ print l
    hClose file
    let loop list = do
        enter <- getLine
        clearScreen
        let new = [applyRuleForlLine board values | board <- list | values <- getSums list]
        mapM_ print new
        loop new
    loop l

applyRuleForlLine :: String -> [Int] -> String
applyRuleForlLine b v = [applyBasicValidationRule c i | c <- b | i <- v]

applyBasicValidationRule :: Char -> Int -> Char
applyBasicValidationRule b v
                        | (b == toChar Dead) && (v == 3) = toChar Alive
                        | (b == toChar Alive) && (v < 2) = toChar Dead
                        | (b == toChar Alive) && (v > 3) = toChar Dead
                        | (b == toChar Alive) && ((v == 2) || (v == 3)) = toChar Alive
                        | otherwise = b