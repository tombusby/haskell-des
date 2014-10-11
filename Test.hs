module Test where

import GlobalTypes
import RoundPermutations

import DebugDisplay

import Data.Word
import System.Random

-- Random Number Generator

randomRIOs :: Random a => (a, a) -> IO [a]
randomRIOs range =
	getStdRandom $ \g -> let (a, b) = split g in (randomRs range a, b)

-- Test Data

constantBinaryBlock :: [Word8]
constantBinaryBlock = replicate 8 (170 :: Word8)

randomBinaryBlock :: IO [Word8]
randomBinaryBlock = do
  ns <- randomRIOs (0 :: Word8, 255 :: Word8)
  return $ take 8 ns

-- Tests

testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData :: IO ()
testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData = do
	b <- return constantBinaryBlock
	b' <- return $ finalPermutation . initialPermutation $ b
	c <- randomBinaryBlock
	c' <- return $ finalPermutation . initialPermutation $ c
	assertb <- return $ b == b'
	assertc <- return $ c == c'
	putStr "testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData\n\n"
	putStr "Test that 170*8 btye block returns itself with inital then final permute => "
	putStrLn $ show assertb
	putStr "Test that random byte block returns itself with inital then final permute => "
	putStrLn $ show assertc
	putStr "\n"

-- Main function

main :: IO ()
main = do
	testThatApplyingInitialPermutationThenFinalPermutationReturnsTheInputData
