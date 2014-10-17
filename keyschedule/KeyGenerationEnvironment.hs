module KeyGenerationEnvironment (KeygenEnv, keygenDefaultEnv, processKeygenArgs) where

import Text.Read(readMaybe)

import Global(Error, Arg, Rounds, KeyFilename)

type KeygenEnv = (Rounds, KeyFilename, [Error])

keygenDefaultEnv :: KeygenEnv
keygenDefaultEnv = (1, "key.txt", [])

setKeygenEnvRounds :: KeygenEnv -> Rounds -> KeygenEnv
setKeygenEnvRounds (_, k, e) r = (r, k, e)

setKeygenEnvKeyFilename :: KeygenEnv -> KeyFilename -> KeygenEnv
setKeygenEnvKeyFilename (r, _, e) k = (r, k, e)

addErrorToKeygenEnv :: KeygenEnv -> Error -> KeygenEnv
addErrorToKeygenEnv (r, k, e) e' = (r, k, e ++ [e'])

processKeygenArgs :: KeygenEnv -> [Arg] -> KeygenEnv
processKeygenArgs env [] = env
processKeygenArgs env (a:as) = processKeygenArgs (modifyEnv env a) as
	where
		modifyEnv :: KeygenEnv -> Arg -> KeygenEnv
		modifyEnv env a = case a of
			('-':'r':'=':tail) -> case readMaybe tail :: Maybe Int of
				Just rounds -> setKeygenEnvRounds env rounds
				Nothing -> addErrorToKeygenEnv env $ a ++ " does not specify a valid number of rounds"
			('-':'k':'=':tail) -> setKeygenEnvKeyFilename env tail
			otherwise -> addErrorToKeygenEnv env $ a ++ " is not a valid flag"
