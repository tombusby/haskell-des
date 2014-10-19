module Encrypt (performEncrypt) where

import Global(Error, Rounds, KeyFilename, InputFilename, OutputFilename)
import Control.Exception(SomeException, try)
import Data.ByteString(ByteString, unpack)
import qualified Data.ByteString as BS(readFile)
import Data.ByteString.Base64(decodeLenient)

performEncrypt :: KeyFilename -> InputFilename -> OutputFilename -> IO ()
performEncrypt k i o = do
	maybeKey <- getKeyFromFile k
	case maybeKey of
		Nothing -> putStrLn $ "We were unable to find and/or read from a key at " ++ k
		Just key -> putStrLn "Key read from file and decoded from Base64"

getKeyFromFile :: KeyFilename -> IO (Maybe ByteString)
getKeyFromFile keyFilename = do
	fileRead <- try (BS.readFile keyFilename) :: IO (Either SomeException ByteString)
	case fileRead of
		Left _ -> return Nothing
		Right key -> return . Just . decodeLenient $ key
