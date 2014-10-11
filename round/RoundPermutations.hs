module RoundPermutations where

import GlobalTypes
import Utilities
import RoundMappings

import Data.Word

initialPermutation :: [Word8] -> [Word8]
initialPermutation bs = listPermute ipMapping bs blank64BitBlock

finalPermutation :: [Word8] -> [Word8]
finalPermutation bs = listPermute fpMapping bs blank64BitBlock
