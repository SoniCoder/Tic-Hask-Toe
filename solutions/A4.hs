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

isWinningLine = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined