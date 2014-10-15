module Main where

import System.Environment(getArgs, getProgName)
import System.IO(hPutStrLn, stderr)

type Command = String
type Arg = String


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

-- TODO
performKeygenAction :: [Arg] -> IO ()
performKeygenAction args = putStrLn $ "keygen " ++ (show args)

-- TODO
performEncryptAction :: [Arg] -> IO ()
performEncryptAction args = putStrLn $ "encrypt " ++ (show args)

-- TODO
performDecryptAction :: [Arg] -> IO ()
performDecryptAction args = putStrLn $ "decrypt " ++ (show args)


-- Utility Functions

printError :: String -> IO ()
printError s = hPutStrLn stderr $ "Error: " ++ s

