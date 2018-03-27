{-# LANGUAGE ParallelListComp #-}
module Main where
import System.IO
import Control.Monad
import Data.List 
import Sum
import Rules
import System.Posix.Unistd

main = do
    clearScreen
    putStr "choose option:\nM - manualy trigger next step\nA - automatically go to next step\n"
    option <- getLine
    clearScreen
    file <- openFile "./resources/board.txt" ReadMode
    content <- hGetContents file
    let l = lines content
    mapM_ print l
    hClose file
    let loop list = do
        if option == "M"
            then do
                getLine
                usleep 0
            else usleep 200000
        clearScreen
        let new = [applyRuleForlLine board values | board <- list | values <- getSums list]
        mapM_ print new
        loop new
    loop l

clearScreen = putStr "\ESC[2J"