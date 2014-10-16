module Global (Error, Action, Arg, Rounds, KeyFilename, Mapping, keyLengthInBytes) where

type Error = String
type Action = String
type Arg = String
type Rounds = Int
type KeyFilename = String
type Mapping = (Int, Int)

keyLengthInBytes :: Int
keyLengthInBytes = 7
