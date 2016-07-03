import qualified Data.Vector as V
import Data.Functor

type SudokuBoard = V.Vector (V.Vector Int)

boardFromList :: [[Int]] -> SudokuBoard
boardFromList = V.fromList . (V.fromList <$>)

n = 9  -- Board size
s = 3  -- = square size = sqrt(n) 

testBoard :: SudokuBoard
testBoard = boardFromList [[0,0,3,0,2,0,6,0,0],
						   [9,0,0,3,0,5,0,0,1],
						   [0,0,1,8,0,6,4,0,0],
						   [0,0,8,1,0,2,9,0,0],
						   [7,0,0,0,0,0,0,0,8],
						   [0,0,6,7,0,8,2,0,0],
						   [0,0,2,6,0,9,5,0,0],
						   [8,0,0,2,0,3,0,0,9],
						   [0,0,5,0,1,0,3,0,0]]

-- Taking the encodings from this paper:
-- http://www.cs.cmu.edu/~hjain/papers/sudoku-as-SAT.pdf
type Row = Int
type Col = Int
type Val = Int

type CellEncoding = (Row, Col, Val)
data UnitEncoding = Unit CellEncoding | Neg CellEncoding  -- Represents boolean negation of clauses.
type Clause = V.Vector UnitEncoding

-- A bunch of different sets of clauses (constraints). 
-- The ones ending with 'D' encode definedness, 
-- the ones ending with 'U' encode uniqueness

cellD :: [Clause]
cellD = [ V.fromList [Unit (r,c,v) | v <- [1..n]] | r <- [1..n], c <- [1..n] ]

cellU :: [Clause]
cellU = [ V.fromList [Neg (r,c,vi), Neg (r,c,vj)] | r <- [1..n], c <- [1..n], vi <- [1..n-1], vj <- [vi + 1 .. n] ]

rowD :: [Clause]
rowD = [ V.fromList [Unit (r,c,v) | c <- [1..n]] | r <- [1..n], v <- [1..n] ]

rowU :: [Clause]
rowU = [ V.fromList [Neg (r,ci,v), Neg (r,cj,v)] | r <- [1..n], v <- [1..n], ci <- [1..n-1], cj <- [ci + 1 .. n] ]

colD :: [Clause]
colD = [ V.fromList [Unit (r,c,v) | r <- [1..n]] | c <- [1..n], v <- [1..n] ]

colU :: [Clause]
colU = [ V.fromList [Neg (ri,c,v), Neg (rj,c,v)] | c <- [1..n], v <- [1..n], ri <- [1..n-1], rj <- [ri + 1 .. n] ]

blockD :: [Clause]
blockD = [ V.fromList [ Unit (roffs * s + r, coffs * s + c, v) | r <- [1..s], c <- [1..s]] | roffs <- [1..s], coffs <- [1..s], v <- [1..n]]

blockU :: [Clause]
blockU = [ V.fromList [Neg (roffs * s + (r `mod` s), coffs * s + (r `mod` s), v), Neg (roffs * s + (r `mod` s), coffs * s + (c `mod` s), v) ] 
		 | roffs <- [1..s], coffs <- [1..s], v <- [1..n], r <- [1..n], c <- [r+1 .. n]]

all :: [Clause]
all = cellD ++ cellU ++ rowD ++ rowU ++ colD ++ colU ++ blockD ++ blockU