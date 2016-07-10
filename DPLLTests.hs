module DPLLTests (
	runAllTests
) where

import Test.HUnit
import Data.Functor

import Data.Map as M

import DPLL

-- some helper functions for construction/testing
n :: Symbol -> Literal
n c = (Neg, c)

p :: Symbol -> Literal
p c = (Pos, c)

tryToSolve :: Clauses -> Maybe Model
tryToSolve clauses = dpll clauses (concat $ symsbolsIn <$> clauses) M.empty
	where 
		symsbolsIn clause = snd <$> clause

tautology = TestCase $ assertEqual "tautology" actual expected
	where
		actual = tryToSolve [[n 1, p 1]]
		expected = Just $ M.fromList [(1, True)]

unsolvable = TestCase $ assertEqual "unsolvable" actual Nothing
	where actual = tryToSolve [[p 1], [n 1]]

implication = TestCase $ assertEqual "implication" actual expected
	where 
		actual = tryToSolve [[n 1, p 2], [p 1]]
		expected = Just $ M.fromList [(1, True), (2, True)]

pureSoln = TestCase $ assertEqual "pure solution" actual expected
	where
		actual = tryToSolve [[n 1, p 2, n 3], [p 1, p 2, n 3], [n 1, p 2, p 3]]
		expected = Just $ M.fromList[(2, True)]

unitCascade = TestCase $ assertEqual "unit cascade" actual expected
	where
		actual = tryToSolve [[n 1, n 2, n 3, p 4], [n 1, n 2, p 3], [n 1, p 2], [p 1]]
		expected = Just $ M.fromList [(1, True), (2, True), (3, True), (4, True)]

circle = TestCase $ assertEqual "circle" actual expected
	where
		actual = tryToSolve [[n 1, p 2], [n 2, p 3], [n 3, p 1]]
		expected = Just $ M.fromList [(1, True), (2, True), (3, True)]

allTests = TestList [tautology, unsolvable, implication, pureSoln, unitCascade, circle]

runAllTests = runTestTT allTests