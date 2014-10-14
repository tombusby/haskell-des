module Main where

i--mport GlobalTypes
--import RoundPermutations

import System.Environment(getArgs)


main :: IO ()
main = do
	args <- getArgs
	putStrLn $ show args
