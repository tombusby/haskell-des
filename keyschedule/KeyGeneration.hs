module KeyGeneration (createKeyfile, generateKey) where

import Data.Word(Word8)
import Data.ByteString(ByteString, pack)
import qualified Data.ByteString as BS(writeFile)
import Data.ByteString.Base64(encode)

import Global(Rounds, KeyFilename, keyLengthInBytes)
import Utilities(randomRIOs)

-- Key Generation Control Structure

generateKey :: Rounds -> IO ByteString
generateKey rounds = do
	ns <- randomRIOs (0 :: Word8, 255 :: Word8)
	return . pack . concat $ [take keyLengthInBytes ns | n <- [1..rounds]]

createKeyfile :: Rounds -> KeyFilename -> IO ()
createKeyfile rounds keyFilename = do
	key <- generateKey rounds
	base64key <- return . encode $ key
	BS.writeFile keyFilename base64key
