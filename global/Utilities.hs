module Utilities (blank64BitBlock, fixLittleEndian, testBitAt, setBitAt, permute, listPermute, printError, printErrors, randomRIOs) where

--import qualified Data.ByteString as BL
import Data.Word(Word8)
import Data.Bits(Bits, testBit, setBit, bitSizeMaybe)
import Data.Maybe(fromJust)
import System.IO(hPutStrLn, stderr)
import System.Random(Random, getStdRandom, randomRs, split)

import Global(Mapping)

-- Binary Manipulation

blank64BitBlock :: [Word8]
blank64BitBlock = replicate 8 (0 :: Word8)

fixLittleEndian :: Bits a => a -> Int -> Int
fixLittleEndian b n = (fromJust . bitSizeMaybe $ b) - n - 1

testBitAt :: Bits a => Int -> a -> Bool
testBitAt n b = let n' = fixLittleEndian b n in testBit b n'

setBitAt :: Bits a => Int -> a -> a
setBitAt n b = let n' = fixLittleEndian b n in setBit b n'

permute :: Mapping -> [Word8] -> [Word8] -> [Word8]
permute (f,t) is os = xs ++ (y':ys)
	where
		fx = f `div` 8
		fy = f `mod` 8
		tx = t `div` 8
		ty = t `mod` 8
		i = is !! fx
		(xs, y:ys) = splitAt tx os
		y' = if testBitAt fy i then setBitAt ty y else y

listPermute :: [Mapping] -> [Word8] -> [Word8] -> [Word8]
listPermute [] _ os = os
listPermute (m:ms) is os = listPermute ms is $ permute m is os

-- Error IO

printError :: Error -> IO ()
printError s = hPutStrLn stderr $ "Error: " ++ s

printErrors :: String -> [Error] -> IO ()
printErrors s es = do
	printError $ "There was a problem " ++ s ++ ":\n"
	printErrors' es
	where
		printErrors' :: [Error] -> IO ()
		printErrors' [] = return ()
		printErrors' (e:es) = do
			hPutStrLn stderr e
			printErrors' es

-- Random Number Generator

randomRIOs :: Random a => (a, a) -> IO [a]
randomRIOs range =
	getStdRandom $ \g -> let (a, b) = split g in (randomRs range a, b)

