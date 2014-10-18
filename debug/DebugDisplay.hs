module DebugDisplay (toBinaryRepresentation) where

import Utilities(testBitAt)

import Data.Word(Word8)
import Data.Bits(Bits)

-- Useful Debug Display Functions

toBinaryRepresentation :: Bits a => [a] -> String
toBinaryRepresentation [] = []
toBinaryRepresentation (x:xs) = (concat [show $ boolToInt . testBitAt n $ x | n <- [0..7]]) ++ toBinaryRepresentation xs
	where
		boolToInt :: Bool -> Int
		boolToInt False = 0
		boolToInt True = 1
