module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares = map showSquare 

-- Q#03

dropFirstCol :: Board -> [[Square]]
dropFirstCol = map tail

-- Q#04

dropLastCol :: Board -> [[Square]]
dropLastCol = map init

--Q#05

formatRows :: [Row] -> [String]
formatRows rs = map (\r -> formatLine(showSquares r)) rs 

-- Q#06

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p ss = null (filter (/= p) ss) 


-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine :: Player -> Line -> Bool
isWinningLine p [] = False 
isWinningLine p l = foldr (\x acc -> x == p && acc) True l

-- Q#08

hasWon :: Player -> Board -> Bool
hasWon p b = foldr (\l acc -> acc || isWinningLine p l) False (getAllLines b) 

-- Q#09

getGameState :: Board -> GameState
getGameState b
    | hasWon X b = W_X
    | hasWon O b = W_O
    | isTied b = T
    | otherwise = IP


playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState nb, nb)
  where nb = putSquare p b m 


-- Q#10

prependRowIndices :: [String] -> [String]
prependRowIndices ss = zipWith (:) ['A'..] ss

-- Q#11

formatBoard :: Board -> String
formatBoard b = unlines $ _HEADER_ : (prependRowIndices . formatRows) b