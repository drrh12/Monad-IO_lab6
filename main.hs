module Main where

import Data.List
import System.IO
import System.IO.Error
import Control.Exception 
import System.Environment

import PersonType
import HelpFunctions
import Functions 
import FileFunctions
import QueryFunctions
import ExceptionsFunctions
        
main = do
    args <- getArgs
    inFile <- tryReadFilename (getInput args) `catch` handlerFilename defaultInputFile
    outFile <- tryReadFilename (getOutput args) `catch` handlerFilename defaultOutputFile
    let help = needHelp False False args 
    if help 
        then putStrLn "Usage: ./main [-i<input file>] [-o<output file>] [-?]"
        else do 
            mainFunction [] inFile outFile
            return ()

getInput :: [String] -> String
getInput [] = defaultInputFile
getInput (arg:args) = 
    case arg of
        ('-':'i':file) -> file
        _ -> getInput args 

getOutput :: [String] -> String
getOutput [] = defaultOutputFile
getOutput (arg:args) = 
    case arg of
        ('-':'o':file) -> file
        _ -> getOutput args 

needHelp :: Bool -> Bool -> [String] -> Bool
needHelp _ _ [] = False
needHelp inp outp (arg:args) = 
    case arg of
        ('-':'i':_) -> inp == False && needHelp True outp args
        ('-':'o':_) -> outp == False && needHelp inp True args 
        _ -> True

mainFunction :: [Person] -> String -> String -> IO [Person]
mainFunction list inFile outFile = do 
    menu
    item <- enterInRange 1 8
    newList <- case item of
            1 -> addPerson list
            2 -> editPerson list
            3 -> deletePerson list
            4 -> printToFile list outFile
            5 -> readFromFile list inFile
            6 -> mostCommonSurname list
            7 -> getWithBirthday list 
            8 -> exit list
    if item /= 8 then mainFunction newList inFile outFile else return newList

menu :: IO ()
menu = do 
    putStrLn "\nSelect a menu item"
    putStrLn "1. Add a new person to the list."
    putStrLn "2. Edit a person's info."
    putStrLn "3. Delete a person from the list."
    putStrLn "4. Print all people to file."
    putStrLn "5. Read people from file."
    putStrLn "6. Get most common surname(s)."
    putStrLn "7. Get people with some specific birthday."
    putStrLn "8. Exit."

exit :: [Person] -> IO [Person]
exit list = do 
  let newList = []
  return newList 