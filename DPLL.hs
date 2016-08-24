module DPLL (
	Symbol, Symbols,
	Sign (Pos, Neg),
	Literal,
	Clause,	Clauses,
	Model,
	dpll
) where

import Data.Maybe
import Data.Functor
import Control.Applicative

import qualified Data.Map as M

-- Ideally this would be anything of type Ord, but I realised that after writing all the rest of this,
-- And I'm not in a mood to refactor all of it.
type Symbol = Int
type Symbols = [Symbol]

-- Indicates whether a symbol is negated or not.
data Sign = Pos | Neg
	deriving (Eq, Show)
type Literal = (Sign, Symbol)
type Clause = [Literal]
type Clauses = [Clause]

-- A map from symbols to their boolean value.
-- The absense of a symbol from the map indicates it is unassigned.
type Model = M.Map Symbol Bool

-- Fairly standard DPLL algorithm (https://en.wikipedia.org/wiki/DPLL_algorithm)
-- Return of Nothing indicates no consistent assignment found.
-- Return value of (Just model) indicates a consistent model.
dpll :: Clauses -> Symbols -> Model -> Maybe Model
dpll clauses symbols model
	| all (clauseIsConsistent model) clauses   = Just model
	| any (clauseIsInconsistent model) clauses = Nothing
	| model' /= model = dpll refined (undefinedSymbols model' symbols) model'
	| otherwise = tryNextSymbolAs True <|> tryNextSymbolAs False
	where
		refined  = refineClause model <$> clauses
		withPure = pureAssign refined
		withUnit = unitProp refined
		model'   = M.unions [model, withPure, withUnit]
		undef    = undefinedSymbols model symbols
		tryNextSymbolAs v = dpll clauses (tail undef) (M.insert (head undef) v model)

undefinedSymbols :: Model -> Symbols -> Symbols
undefinedSymbols model = filter $ flip M.notMember model

clauseIsConsistent :: Model -> Clause -> Bool
clauseIsConsistent model literals = or $ mapMaybe (trueUnder model) literals

clauseIsInconsistent :: Model -> Clause -> Bool
clauseIsInconsistent model literals = all not $ trueishUnder model <$> literals

-- Checks if a literal is either True or unset under the model.
-- If it is unset, it the function will return True.
trueishUnder :: Model -> Literal -> Bool
trueishUnder model (sign, symbol) = apply sign $ M.findWithDefault (apply sign True) symbol model

trueUnder :: Model -> Literal -> Maybe Bool
trueUnder model (sign, symbol) = apply sign <$> M.lookup symbol model

apply :: Sign -> Bool -> Bool
apply Pos = id
apply Neg = not

-- Useful way to combine (a -> Bool) functions.
-- If I was doing this a lot, would probably want to use liftM2
(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) f g a = (f a) || (g a)

-- Remove literals from clause if they cannot be true (i.e. already set to their opposite value)
-- or if they are unncessary (unset, and another literal already makes the clause true)
refineClause :: Model -> Clause -> Clause
refineClause model literals = filter (oppositeDefined .||. unncessary) literals
	where
		-- Return False iff the literal is already set to its opposite value
		oppositeDefined :: Literal -> Bool
		oppositeDefined = (fromMaybe True) . (trueUnder model)

		-- Return False iff the clause is consistent, and the value is undefined.
		unncessary :: Literal -> Bool
		unncessary lit = clauseIsConsistent model literals && M.notMember (snd lit) model

-- Assign 'pure' literals to their appropriate value
pureAssign :: Clauses -> Model
pureAssign clauses = (\x -> apply x True) <$> fromJust <$> pureLiterals
	where
		pureLiterals = M.filter isJust $ gatherSigns clauses

-- Helper function for keeping track of whether we've seen both signs for a particular symbol
which :: Eq a => Maybe a -> Maybe a -> Maybe a
which Nothing _ = Nothing
which _ Nothing = Nothing
which (Just a) (Just b)
	| a == b = Just a
	| otherwise = Nothing

-- Creates a mapping of pure symbols to the sign they're pure under
gatherSigns :: Clauses -> M.Map Symbol (Maybe Sign)
gatherSigns clauses = M.fromListWith which asMaps
	where
		asMaps = do
			clause 		<- clauses
			(sign, sym) <- clause
			return (sym, Just sign)

-- Performs unit propagation.
unitProp :: [Clause] -> Model
unitProp clauses = M.fromList $ asKv <$> unitClauses
	where
		asKv (sign, sym) = (sym, apply sign True) 
		unitClauses = concat $ filter isUnit clauses
		isUnit [x] = True
		isUnit _   = False

