module Sudoku (
	SudokuBoard,
	Row, Col, Val, Symbol,
	Literal,
	Clause, Clauses,
	boardFromList,
	boardToClauses,
	defaultClauses,
	isValidSolution
) where

import Data.Functor
import Control.Applicative
import Data.Maybe

import Data.Vector (Vector, (!), fromList)
import qualified Data.Vector as V

import DPLL (Sign(Pos, Neg))

type SudokuBoard = Vector (Vector Int)

boardFromList :: [[Int]] -> SudokuBoard
boardFromList = fromList . (fromList <$>)

-- Taking the encodings from this paper:
-- http://www.cs.cmu.edu/~hjain/papers/sudoku-as-SAT.pdf
type Row = Int
type Col = Int
type Val = Int
type Symbol = (Row, Col, Val)

type Literal = (Sign, Row, Col, Val)
type Clause = [Literal]
type Clauses = [Clause]

-- Integer square root
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

-- A bunch of different sets of Clauses (constraints). 
-- The ones ending with 'D' encode definedness, 
-- the ones ending with 'U' encode uniqueness

cellD :: Int -> Clauses
cellD n = [[(Pos, r,c,v) 
		| v <- [1..n]] 
		| r <- [1..n], c <- [1..n]]

cellU :: Int -> Clauses
cellU n = [[(Neg, r,c,vi), (Neg, r,c,vj)]
		| r <- [1..n], c <- [1..n], vi <- [1..n-1], vj <- [vi + 1 .. n]]

rowD :: Int -> Clauses
rowD n  = [[(Pos, r,c,v) 
		| c <- [1..n]]
		| r <- [1..n], v <- [1..n]]

rowU :: Int -> Clauses
rowU n  = [[(Neg, r,ci,v), (Neg, r,cj,v)] 
		| r <- [1..n], v <- [1..n], ci <- [1..n-1], cj <- [ci + 1 .. n]]

colD :: Int -> Clauses
colD n  = [[(Pos, r,c,v) 
		| r <- [1..n]] 
		| c <- [1..n], v <- [1..n]]

colU :: Int -> Clauses
colU n  = [[(Neg, ri,c,v), (Neg, rj,c,v)] 
		| c <- [1..n], v <- [1..n], ri <- [1..n-1], rj <- [ri + 1 .. n]]

blockD :: Int -> Clauses
blockD n  = [[ (Pos, roffs * s + r, coffs * s + c, v) 
		  | r <- [1..s], c <- [1..s]]
	  	  | roffs <- [0..s-1], coffs <- [0..s-1], v <- [1..n]]
  	where s = isqrt n

blockU :: Int -> Clauses
blockU n = [[(Neg, roffs * s + r, coffs * s + r, v), 
			 (Neg, roffs * s + r, coffs * s + c, v) ] 
		 | roffs <- [0..s-1], coffs <- [0..s-1], v <- [1..n], r <- [1..s], c <- [r+1 .. s]]
	where s = isqrt n

defaultClauses :: Int ->  Clauses
defaultClauses n = concat $ [cellD, cellU, rowD, rowU, colD, colU, blockD, blockU] <*> [n]

boardToClauses :: SudokuBoard -> Clauses
boardToClauses board = pure <$> (catMaybes literals)
	where 
		n = V.length board
		literals = [positionToConstraint board r c | r <- [1..n], c <- [1..n]]

positionToConstraint :: SudokuBoard -> Row -> Col -> Maybe Literal
positionToConstraint b r c
	| v == 0    = Nothing
	| otherwise = Just (Pos, r, c, v)
	where v = b ! (r-1) ! (c-1)

isValidSolution :: SudokuBoard -> Bool
isValidSolution board = (all rowCheckSum toN) && (all colCheckSum toN) && (and blocksCheck)
	where 
		n = V.length board
		s = isqrt n
		toN = [1..n]
		chksum = sum toN
		cell r c = board ! (r - 1) ! (c - 1)
		allCellsDefined = [ cell r c >= 1 && cell r c <= n | r <- toN, c <- toN]
		rowCheckSum r = sum [ cell r c | c <- toN] == chksum
		colCheckSum c = sum [ cell r c | r <- toN] == chksum
		blockCheckSum roff coff = sum [cell (roff * s + r) (coff * s + c) | r <- [1..s], c <- [1..s]] == chksum
		blocksCheck = [blockCheckSum roff coff | roff <- [0..s-1], coff <- [0..s-1]] 