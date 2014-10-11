module Utilities where

import GlobalTypes

import qualified Data.ByteString as BL
import Data.Word
import Data.Bits
import Data.Maybe

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
