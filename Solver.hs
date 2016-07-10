module Solver (
	solve
) where

import Data.Functor
import Data.List

import Data.Bimap (Bimap, (!), (!>), fromAscPairList)
import qualified Data.Vector as V
import qualified Data.Map as M

import Sudoku as Su
import DPLL as SAT

type SymbolMap = Bimap Su.Symbol SAT.Symbol

satSymbols n = [1 .. n*n*n]

bijection :: Int -> SymbolMap
bijection n = fromAscPairList $ zip [(r,c,v) | r <- [1..n], c <- [1..n], v <- [1..n]] (satSymbols n)

toSatFormat :: SymbolMap -> Su.Clauses -> SAT.Clauses
toSatFormat twoWayMap clauses = [ [ convLit lit | lit <- clause ] | clause <- clauses ]
	where
		convLit :: Su.Literal -> SAT.Literal
		convLit (sign, r, c, v) = (sign, twoWayMap ! (r,c,v))

solve :: Su.SudokuBoard -> Maybe Su.SudokuBoard
solve board = modelToBoard twoWayMap <$> SAT.dpll satClauses (satSymbols n) M.empty
	where
		n = (V.length board)
		twoWayMap = (bijection n)
		satClauses = toSatFormat twoWayMap $ (Su.boardToClauses board) ++ (Su.defaultClauses n)

modelToBoard :: SymbolMap -> SAT.Model -> Su.SudokuBoard
modelToBoard twoWayMap model = Su.boardFromList $ [ [ v | (_,_,v) <- row] | row <- rows]
	where 
		listOfPositions = sort $ ((!>) twoWayMap) <$> M.keys (M.filter id model)
		rows = groupBy (\x y -> fst3 x == fst3 y) listOfPositions
		fst3 (a,_,_) = a
