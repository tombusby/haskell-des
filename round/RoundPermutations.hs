module RoundPermutations (initialPermutation, finalPermutation) where

import Utilities(blank64BitBlock, listPermute)
import RoundMappings(ipMapping, fpMapping)

import Data.Word(Word8)


initialPermutation :: [Word8] -> [Word8]
initialPermutation bs = listPermute ipMapping bs blank64BitBlock

finalPermutation :: [Word8] -> [Word8]
finalPermutation bs = listPermute fpMapping bs blank64BitBlock
