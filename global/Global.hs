module Global (Error, Action, Arg, Rounds, KeyFilename, InputFilename, OutputFilename, Mapping, keyLengthInBytes) where

type Error = String
type Action = String
type Arg = String
type Rounds = Int
type KeyFilename = String
type InputFilename = String
type OutputFilename = String
type Mapping = (Int, Int)

-- Key Generation Constants

keyLengthInBytes :: Int
keyLengthInBytes = 8
