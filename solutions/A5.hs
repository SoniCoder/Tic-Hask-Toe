{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Use <$>" #-}
module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)
import Data.Functor ((<&>))

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard b = putStr $ formatBoard b

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"


printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen


firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ <&> getFirstPlayer

-- Q#04

getMove :: Board -> IO Move
getMove b = do
    m <- getLine <&> stringToMove
    if isValidMove b m
        then return m
        else putStrLn "Invalid move! Try again" >> getMove b

-- Q#05

play :: Board -> Player -> IO ()
play b p = do
    printLogo
    printBoard b
    putStrLn $ promptPlayer p
    m <- getMove b
    case playMove p b m of
        (IP, b') -> play b' (switchPlayer p)
        (gs, b') -> printBoard b' >> putStrLn (showGameState gs)

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo :: IO ()
printLogoDo = do
    f <- readFile _LOGO_PATH_ 
    putStrLn f

-- Q#08

firstPlayerDo :: IO Player
firstPlayerDo = do
    b <- _RANDOM_BOOL_
    return $ getFirstPlayer b

-- Q#09

getMoveDo :: Board -> IO Move
getMoveDo b = do
    m <- getLine <&> stringToMove
    if isValidMove b m
        then return m
        else do 
            putStrLn "Invalid move! Try again"
            getMoveDo b

-- Q#10

playDo :: Board -> Player -> IO ()
playDo b p = do
    printLogo
    printBoard b
    putStrLn $ promptPlayer p
    m <- getMove b
    case playMove p b m of
        (IP, b') -> playDo b' (switchPlayer p)
        (gs, b') -> printBoard b' >> putStrLn (showGameState gs)