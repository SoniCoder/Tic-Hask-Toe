{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
promptPlayer p =
    concat ["Player ",show p, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0 .. _SIZE_ - 1]

-- Q#03

isDigit :: Char -> Bool
isDigit c =
    c `elem` ['0' .. '9']


readDigit :: Char -> Int
readDigit d
    | isDigit d = read [d]
    | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ E


_EMPTY_BOARD_ :: [Row]
_EMPTY_BOARD_ = replicate  _SIZE_ _EMPTY_ROW_

-- Q#05
-- Note: Actually you also should check that there is no win
isTied :: Board -> Bool
isTied b
    | E `elem` concat b = False
    | otherwise = True


_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings ls = zip ['A' ..] ls 

-- Q#07

formatLine :: [String] -> String
formatLine ls = concat [_SEP_, intercalate _SEP_ ls, _SEP_]

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined