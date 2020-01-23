module FileFunctions where

import Control.Exception

import PersonType
import HelpFunctions
import ExceptionsFunctions

printToFile :: [Person] -> String -> IO [Person]
printToFile list path = do 
        let array = map (\p -> (lastName p, birthday p, speciality p)) list 
        writeFile path (show array)
        putStrLn "\nPerson printed into the file."
        return list



readFromFile :: [Person] -> String -> IO [Person]
readFromFile list path = do
        str_output <- readFile path `catch` handlerReadFile
        let array = (read str_output) :: [(String, String, String)]
        let newList = convertToPersonList array
        putStrLn ("List inserted: " ++ show newList) 
        return newList

