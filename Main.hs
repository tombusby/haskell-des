module Main (main) where

import System.Environment(getArgs, getProgName)

import Global(Error, Action, Arg)
import Utilities(printError, printErrors)
import KeyGenerationEnvironment(keygenDefaultEnv, processKeygenArgs)
import KeyGeneration(createKeyfile)
import EncryptEnvironment(encryptDefaultEnv, processEncryptArgs)
import Encrypt(performEncrypt)


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

processArgs :: Action -> [Arg] -> IO ()
processArgs "keygen" args = performKeygenAction args
processArgs "encrypt" args = performEncryptAction args
processArgs "decrypt" args = performDecryptAction args
processArgs c _ = do
	printError $ c ++ " is not a valid action"
	putStr "\n"
	printUsage

performKeygenAction :: [Arg] -> IO ()
performKeygenAction args = do
	(rounds, keyFilename, argErrors) <- return $ processKeygenArgs keygenDefaultEnv args
	if length(argErrors) > 0 then do
	 	printErrors "processing command line arguments" argErrors
		putStr "\n"
		printUsage
	 else
	 	createKeyfile rounds keyFilename

performEncryptAction :: [Arg] -> IO ()
performEncryptAction args = do
	(keyFilename, inputFilename, outputFilename, argErrors) <- return $ processEncryptArgs encryptDefaultEnv args
	if length(argErrors) > 0 then do
	 	printErrors "processing command line arguments" argErrors
		putStr "\n"
		printUsage
	 else
	 	performEncrypt keyFilename inputFilename outputFilename

-- TODO
performDecryptAction :: [Arg] -> IO ()
performDecryptAction args = putStrLn $ "decrypt " ++ (show args)

