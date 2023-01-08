module Main where

import A1
import A2
import A3
import A4
import A5

main :: IO ()
main = do
    putStrLn "Welcome to Tic-Tac-Toe!"
    play _EMPTY_BOARD_ =<< firstPlayer