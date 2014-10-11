module Experiments where

import qualified Data.ByteString as BL
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Tuple

type Mapping = (Int, Int)


-- Test Data

testData :: [Word8]
testData = replicate 8 (170 :: Word8)


-- Utilities

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


-- Useful Debug Display Functions

toBinaryRepresentation :: Bits a => [a] -> String
toBinaryRepresentation [] = []
toBinaryRepresentation (x:xs) = (concat [show $ boolToInt . testBitAt n $ x | n <- [0..7]]) ++ toBinaryRepresentation xs
	where
		boolToInt :: Bool -> Int
		boolToInt False = 0
		boolToInt True = 1


-- Mappings

ipMapping :: [Mapping]
ipMapping = zip [0..] [57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47,
	39, 31, 23, 15, 7, 56, 48, 40, 32, 24, 16, 8, 0, 58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46,
	38, 30, 2, 14, 6]

fpMapping :: [Mapping]
fpMapping = map swap ipMapping


-- DES functions

initialPermutation :: [Word8] -> [Word8]
initialPermutation bs = listPermute ipMapping bs blank64BitBlock

finalPermutation :: [Word8] -> [Word8]
finalPermutation bs = listPermute fpMapping bs blank64BitBlock




