module Encrypt (performEncrypt) where

import Global(Error, Rounds, KeyFilename, InputFilename, OutputFilename)

performEncrypt :: KeyFilename -> InputFilename -> OutputFilename -> IO ()
performEncrypt k i o = do
	putStrLn "Performing encryption"
