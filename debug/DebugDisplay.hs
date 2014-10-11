module DebugDisplay where

import Utilities

import Data.Word
import Data.Bits

-- Useful Debug Display Functions

toBinaryRepresentation :: Bits a => [a] -> String
toBinaryRepresentation [] = []
toBinaryRepresentation (x:xs) = (concat [show $ boolToInt . testBitAt n $ x | n <- [0..7]]) ++ toBinaryRepresentation xs
	where
		boolToInt :: Bool -> Int
		boolToInt False = 0
		boolToInt True = 1
