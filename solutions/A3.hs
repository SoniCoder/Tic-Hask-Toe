module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x : xs) = show x : showInts xs


_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (sq : sqs) = showSquare sq : showSquares sqs


-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (r : rs) = formatLine (showSquares r): formatRows rs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False  
isColEmpty (s : ss) 0 = s == E
isColEmpty (s : ss) c
    | c >= 0 = isColEmpty ss (c-1)
    | otherwise = False  

-- Q#05

dropFirstCol :: Board -> [[Square]]
dropFirstCol [] = []
dropFirstCol ([] : rs) = [] : dropFirstCol rs
dropFirstCol (r : rs) = tail r : dropFirstCol rs


dropLastCol :: Board -> [[Square]]
dropLastCol [] = []
dropLastCol ([] : rs) = [] : dropLastCol rs
dropLastCol (r : rs) = init r : dropLastCol rs

-- Q#06

getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 ([] : rs) = getDiag1 (dropFirstCol rs)
getDiag1 ((s : ss) : rs) = s : getDiag1 (dropFirstCol rs)



getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 ([]: rs) = getDiag2 (dropLastCol rs)
getDiag2 (r : rs) = last r : getDiag2 (dropLastCol rs)



getAllLines :: Board -> [Line]
getAllLines b = concat [b, transpose b, [getDiag1 b], [getDiag2 b]]

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (r: rs) (0, y) = replaceSquareInRow p y r : rs
putSquare p (r: rs) (x, y) = r : putSquare p rs (x-1, y) 

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices ss =
    postprocess (indexRowStrings ss)
    where postprocess [] = []
          postprocess ((c, s) : css) =  (c:s) : postprocess css

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l =
    go l False
    where go [] acc = acc
          go (s : ss) acc = s == p && go ss True


-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove b m = isMoveInBounds m && isSquareEmpty b m 0
        where isSquareEmpty (r : rs) (x, y) c
                | c == x = isColEmpty r y
                | otherwise = isSquareEmpty rs m (c+1)
              isSquareEmpty [] _ _ = False