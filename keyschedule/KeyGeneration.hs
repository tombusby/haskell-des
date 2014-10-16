module KeyGeneration (createKeyfile) where

import Data.Word(Word8)
import Data.ByteString(ByteString, pack)
import qualified Data.ByteString as BS(writeFile)
import Data.ByteString.Base64(encode)

import Global(Rounds, KeyFilename, keyLengthInBytes)
import Utilities(randomRIOs)

generateKey :: IO ByteString
generateKey = do
  ns <- randomRIOs (0 :: Word8, 255 :: Word8)
  return . encode . pack $ take keyLengthInBytes ns

createKeyfile :: Rounds -> KeyFilename -> IO ()
createKeyfile rounds keyFilename = do
	key <- generateKey
	BS.writeFile keyFilename key
