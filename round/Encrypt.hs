module Encrypt (performEncrypt) where

import Global(Error, Rounds, KeyFilename, InputFilename, OutputFilename)
import Utilities(printErrors)

import Control.Exception(SomeException, try)
import Data.ByteString(ByteString, unpack)
import qualified Data.ByteString as BS(readFile)
import Data.ByteString.Base64(decodeLenient)
import System.Directory(doesFileExist)

-- Encrypt Control Structure

performEncrypt :: KeyFilename -> InputFilename -> OutputFilename -> IO ()
performEncrypt k i o = do
	maybeKey <- getKeyFromFile k
	case maybeKey of
		Nothing -> printErrors "reading the key file" ["We were unable to find and/or read from a key at " ++ k]
		Just key -> performEncrypt2 key i o

performEncrypt2 :: ByteString -> InputFilename -> OutputFilename -> IO ()
performEncrypt2 key i o = do
	inputExists <- doesFileExist i
	if not inputExists then
		printErrors "reading the input file" ["The input file path " ++ i ++ " does not exist"]
	 else
	 	performEncrypt3 key i o

performEncrypt3 :: ByteString -> InputFilename -> OutputFilename -> IO ()
performEncrypt3 key i o = do
	putStrLn "file found, this is a placeholder for where the reading of the file will happen"

-- Utility Functions

-- (Possibly move somewhere more global for use with decrpyt)
getKeyFromFile :: KeyFilename -> IO (Maybe ByteString)
getKeyFromFile keyFilename = do
	fileRead <- try (BS.readFile keyFilename) :: IO (Either SomeException ByteString)
	case fileRead of
		Left _ -> return Nothing
		Right key -> return . Just . decodeLenient $ key


