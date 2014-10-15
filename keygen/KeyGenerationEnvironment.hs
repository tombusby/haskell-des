module KeyGenerationEnvironment (KeygenEnv, keygenDefaultEnv, processKeygenArgs) where

import Text.Read(readMaybe)

import Global(Error, Arg, Rounds, KeyFilename)

type KeygenEnv = (Rounds, KeyFilename, [Error])
type EncryptEnv = () -- TODO
type DecryptEnv = () -- TODO

keygenDefaultEnv :: KeygenEnv
keygenDefaultEnv = (1, "key.txt", [])

setKeygenEnvRounds :: KeygenEnv -> Rounds -> KeygenEnv
setKeygenEnvRounds (_, o, e) r = (r, o, e)

setKeygenEnvKeyFilename :: KeygenEnv -> KeyFilename -> KeygenEnv
setKeygenEnvKeyFilename (r, _, e) o = (r, o, e)

addErrorToKeygenEnv :: KeygenEnv -> Error -> KeygenEnv
addErrorToKeygenEnv (r, o, e) e' = (r, o, e ++ [e'])

processKeygenArgs :: KeygenEnv -> [Arg] -> KeygenEnv
processKeygenArgs env [] = env
processKeygenArgs env (a:as) = processKeygenArgs (modifyEnv env a) as
	where
		modifyEnv :: KeygenEnv -> Arg -> KeygenEnv
		modifyEnv env a = case a of
			('-':'r':'=':tail) -> case readMaybe tail :: Maybe Int of
				Just rounds -> setKeygenEnvRounds env rounds
				Nothing -> addErrorToKeygenEnv env $ a ++ " does not specify a valid number of rounds"
			('-':'o':'=':tail) -> setKeygenEnvKeyFilename env tail
			otherwise -> addErrorToKeygenEnv env $ a ++ " is not a valid flag"