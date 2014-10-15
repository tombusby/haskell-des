module Main where

--import GlobalTypes
--import RoundPermutations

import System.Environment(getArgs)


main :: IO ()
main = do
	args <- getArgs
	putStrLn $ show args
