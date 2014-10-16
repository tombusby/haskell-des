module Global (Error, Command, Arg, Rounds, KeyFilename, Mapping, keyLengthInBytes) where

type Error = String
type Command = String
type Arg = String
type Rounds = Int
type KeyFilename = String
type Mapping = (Int, Int)

keyLengthInBytes :: Int
keyLengthInBytes = 7
