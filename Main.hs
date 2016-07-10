import DPLLTests (runAllTests)
import SudokuTests (runAllTests)

main = do
	putStrLn "Welcome to the Sudoku Solver"
	Main.runAllTests	

runAllTests = do
	putStrLn "Running DPLLTests"
	DPLLTests.runAllTests
	putStrLn "Running Test Sudoku Boards"
	SudokuTests.runAllTests