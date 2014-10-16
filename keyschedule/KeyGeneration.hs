module KeyGeneration (createKeyfile) where

import Data.Word(Word8)
import Data.ByteString(ByteString, pack)
import qualified Data.ByteString as BS(writeFile)
import Data.ByteString.Base64(encode)

import Global(Rounds, KeyFilename, keyLengthInBytes)
import Utilities(randomRIOs)

generateKey :: Rounds -> IO ByteString
generateKey rounds = do
	ns <- randomRIOs (zeroByte, oneByte)
	return . encode . pack . concat . keys $ ns
  	where
  		keys :: [Word8] -> [[Word8]]
  		keys ns = [take keyLengthInBytes ns ++ [zeroByte] | n <- [1..rounds]]
  		zeroByte = 0 :: Word8
  		oneByte = 255 :: Word8

createKeyfile :: Rounds -> KeyFilename -> IO ()
createKeyfile rounds keyFilename = do
	key <- generateKey rounds
	BS.writeFile keyFilename key
