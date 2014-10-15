module Test () where

import RoundPermutations(initialPermutation, finalPermutation)

import TestData(constantBinaryBlock, randomBinaryBlock)
import DebugDisplay(toBinaryRepresentation)


-- Tests

testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData :: IO ()
testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData = do
	-- Show test name
	putStr "testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData\n\n"

	-- Do assertion with fixed data
	b <- return constantBinaryBlock
	b' <- return . finalPermutation . initialPermutation $ b
	assertb <- return $ b == b'

	-- Show assert
	putStr "Test that 170*8 btye block returns itself with inital then final permute => "
	putStrLn $ show assertb
	
	-- Do assertion with random data
	c <- randomBinaryBlock
	c' <- return . finalPermutation . initialPermutation $ c
	assertc <- return $ c == c'

	-- Show assert
	putStr "Test that random byte block returns itself with inital then final permute => "
	putStrLn $ show assertc
	putStr "\n"

-- Main function

main :: IO ()
main = do
	testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData
