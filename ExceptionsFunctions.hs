module ExceptionsFunctions where

import System.IO
import System.IO.Error
import System.Directory

tryReadFilename :: String -> IO String
tryReadFilename path = do 
    ex <- doesFileExist path
    if ex
        then return path 
        else ioError $ ioeSetErrorType err doesNotExistErrorType
    where
        err = (userError $ "File " ++ path ++ " does not exist!")  

handlerFilename :: String -> IOError -> IO String
handlerFilename defaultFile e = do
    if isDoesNotExistError e 
        then putStrLn "File does not exist"
        else putStrLn "Something unexpected happened"
    putStrLn "Using default file"
    return defaultFile 

handlerReadFile :: IOError -> IO String
handlerReadFile e = do 
    if isDoesNotExistError e
        then putStrLn "File does not exist"
        else putStrLn "Something unexpected happened"
    ioError e 
    return "" 