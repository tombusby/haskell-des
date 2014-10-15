module TestData (randomRIOs, constantBinaryBlock, randomBinaryBlock) where

import Data.Word(Word8)

import Utilities(randomRIOs)

-- Test Data

constantBinaryBlock :: [Word8]
constantBinaryBlock = replicate 8 (170 :: Word8)

randomBinaryBlock :: IO [Word8]
randomBinaryBlock = do
  ns <- randomRIOs (0 :: Word8, 255 :: Word8)
  return $ take 8 ns
