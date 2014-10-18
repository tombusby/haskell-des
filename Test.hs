module Test (main) where

import RoundPermutations(initialPermutation, finalPermutation)

import KeyGeneration(generateKey)
import qualified Data.ByteString as BS(length)

import TestData(constantBinaryBlock, randomBinaryBlock)
import DebugDisplay(toBinaryRepresentation) -- not called, but useful to have in scope in ghci

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
	putStr "\tTest that 170*8 btye block returns itself with inital then final permute => "
	putStrLn $ show assertb
	
	-- Do assertion with random data
	c <- randomBinaryBlock
	c' <- return . finalPermutation . initialPermutation $ c
	assertc <- return $ c == c'

	-- Show assert
	putStr "\tTest that random byte block returns itself with inital then final permute => "
	putStrLn $ show assertc
	putStr "\n"

testThatKeysAreGeneratedToTheCorrectLength :: IO ()
testThatKeysAreGeneratedToTheCorrectLength = do
	-- Show test name
	putStr "testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData\n\n"

	-- Get a randomly generated key for 1DES and assert the correct length
	key1 <- generateKey 1
	assert1 <- return $ BS.length key1 == 8

	-- Show assert
	putStr "\tTest that for 1DES generateKey returns a 64 bit bytestring (even though only 56 bits will be used) => "
	putStrLn $ show assert1

	-- Get a randomly generated key for 3DES and assert the correct length
	key3 <- generateKey 3
	assert3 <- return $ BS.length key3 == 24

	-- Show assert
	putStr "\tTest that for 3DES generateKey returns a 192 bit bytestring (even though only 168 bits will be used) => "
	putStrLn $ show assert3
	putStr "\n"

-- Main function

main :: IO ()
main = do
	testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData
	testThatKeysAreGeneratedToTheCorrectLength
