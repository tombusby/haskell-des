module EncryptEnvironment (encryptDefaultEnv, processEncryptArgs) where

import Text.Read(readMaybe)

import Global(Error, Arg, KeyFilename, InputFilename, OutputFilename)

type EncryptEnv = (KeyFilename, InputFilename, OutputFilename, [Error])

encryptDefaultEnv :: EncryptEnv
encryptDefaultEnv = ("key.txt", "", "out.des", [])

setEncryptEnvKeyFilename :: EncryptEnv -> KeyFilename -> EncryptEnv
setEncryptEnvKeyFilename (_, i, o, e) k = (k, i, o, e)

setEncryptEnvInputFilename :: EncryptEnv -> InputFilename -> EncryptEnv
setEncryptEnvInputFilename (k, _, o, e) i = (k, i, o, e)

setEncryptEnvOutputFilename :: EncryptEnv -> OutputFilename -> EncryptEnv
setEncryptEnvOutputFilename (k, i, _, e) o = (k, i, o, e)

addErrorToEncryptEnv :: EncryptEnv -> Error -> EncryptEnv
addErrorToEncryptEnv (k, i, o, e) e' = (k, i, o, e ++ [e'])

processEncryptArgs :: EncryptEnv -> [Arg] -> EncryptEnv
processEncryptArgs env [] = case env of
	(_, "", _, _) -> addErrorToEncryptEnv env "You have not specified an input file"
	otherwise -> env
processEncryptArgs env (a:as) = processEncryptArgs (modifyEnv env a) as
	where
		modifyEnv :: EncryptEnv -> Arg -> EncryptEnv
		modifyEnv env a = case a of
			('-':'k':'=':tail) -> setEncryptEnvKeyFilename env tail
			('-':'i':'=':tail) -> setEncryptEnvInputFilename env tail
			('-':'o':'=':tail) -> setEncryptEnvOutputFilename env tail		
			otherwise -> addErrorToEncryptEnv env $ a ++ " is not a valid flag"
