module Main where

import System.Environment(getArgs, getProgName)
import System.IO(hPutStrLn, stderr)

import Global(Error, Command, Arg)
import KeyGenerationEnvironment(keygenDefaultEnv, processKeygenArgs)
import KeyGeneration(createKeyfile)

-- Utility Functions

printError :: Error -> IO ()
printError s = hPutStrLn stderr $ "Error: " ++ s

printErrors :: String -> [Error] -> IO ()
printErrors s es = do
	printError $ "There was a problem " ++ s ++ ":\n"
	printErrors' es
	where
		printErrors' :: [Error] -> IO ()
		printErrors' [] = return ()
		printErrors' (e:es) = do
			hPutStrLn stderr e
			printErrors' es

-- Main Program Logic

main :: IO ()
main = do
	args <- getArgs
	case args of
		[] -> printUsage
		(a:as) -> processArgs a as

printUsage :: IO ()
printUsage = do
	argv0 <- getProgName
	putStrLn $ "Usage: " ++ argv0 ++ " (todo: specify arguments)"

processArgs :: Command -> [Arg] -> IO ()
processArgs "keygen" args = performKeygenAction args
processArgs "encrypt" args = performEncryptAction args
processArgs "decrypt" args = performDecryptAction args
processArgs c _ = printError $ c ++ " is not a valid action"

performKeygenAction :: [Arg] -> IO ()
performKeygenAction args = do
	(rounds, keyFilename, argErrors) <- return $ processKeygenArgs keygenDefaultEnv args
	if length(argErrors) > 0 then
	 	printErrors "processing command line arguments" argErrors
	 else
	 	createKeyfile rounds keyFilename

-- TODO
performEncryptAction :: [Arg] -> IO ()
performEncryptAction args = putStrLn $ "encrypt " ++ (show args)

-- TODO
performDecryptAction :: [Arg] -> IO ()
performDecryptAction args = putStrLn $ "decrypt " ++ (show args)

