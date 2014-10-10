module Experiments where

import qualified Data.ByteString as BL
import Data.Word
import Data.Bits
import Data.Maybe

type Mapping = (Int, Int)


-- Test Data

testData :: [Word8]
testData = replicate 8 (170 :: Word8)


-- Utilities

blank64BitBlock :: [Word8]
blank64BitBlock = replicate 8 (0 :: Word8)

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

fixLittleEndian :: Bits a => a -> Int -> Int
fixLittleEndian b n = (fromJust . bitSizeMaybe $ b) - n - 1

getBitAt :: Bits a => Int -> a -> Int
getBitAt n b = let n' = fixLittleEndian b n in boolToInt $ testBit b n'

setBitAt :: Bits a => Int -> a -> a
setBitAt n b = let n' = fixLittleEndian b n in setBit b n'

permute :: Bits a => Mapping -> [a] -> [a] -> [a]
permute (f,t) is os = xs ++ [y'] ++ ys
	where
		f' = f `div` 8
		f'' = f `mod` 8
		t' = t `div` 8
		t'' = t `mod` 8
		i = is !! f'
		(xs, y:ys) = splitAt t' os
		y' = if getBitAt f'' i == 1 then setBitAt t'' y else y


-- Useful Debug Display Functions

toBinaryRepresentation :: Bits a => [a] -> String
toBinaryRepresentation [] = []
toBinaryRepresentation (x:xs) = (concat [show $ getBitAt n x | n <- [0..7]]) ++ toBinaryRepresentation xs


-- Mappings

ipMapping :: [Mapping]
ipMapping = zip [0..] [58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 
	40, 32, 24, 16, 8, 57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 
	39, 31, 23, 15, 7]


-- DES functions

initialPermutation :: [Word8] -> [Word8]
initialPermutation bs = listPermute ipMapping bs blank64BitBlock

listPermute :: [Mapping] -> [Word8] -> [Word8] -> [Word8]
listPermute [] _ o = o
listPermute (m:ms) is os = listPermute ms is $ permute m is os




