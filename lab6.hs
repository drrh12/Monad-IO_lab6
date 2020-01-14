import Prelude hiding (Functor, fmap, (<$>), Applicative, pure, (<*>))
import Data.List

data Person = Person {
    lastName :: [Char],
    birthday :: [Char],
    speciality :: String
} 

instance Show Person where
    show (Person lastName date spec) = " " ++ lastName ++ " (" ++ date ++ "): " ++ spec ++ " "


class Functor f where
        fmap :: (a -> b) -> f a -> f b
        (<$>) :: (a -> b) -> f a -> f b
        (<$>) = fmap
        
class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        
main = mainFunction []

mainFunction list = do 
    menu
    item <- enterInRange 1 8
    newList <- case item of
            1 -> addPerson list
            2 -> editPerson list
            3 -> deletePerson list
            4 -> printToFile list
            5 -> readFromFile list
            6 -> mostCommonSurname list
            7 -> getWithBirthday list 
            8 -> exit list
    if item /= 8 then mainFunction newList else return newList

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

enterInRange :: Int -> Int -> IO Int 
enterInRange bottom_val top_val = do 
    putStrLn $ "(enter in range: " ++ show bottom_val ++ " - " ++ show top_val ++ ")"
    str <- getLine 
    let num = read str :: Int 
    if num < bottom_val || num > top_val 
        then do 
            putStrLn "Number out of range"
            enterInRange bottom_val top_val
    else
        return num 

addPerson :: [Person] -> IO [Person]
addPerson list = do
    putStrLn "\nInsert the name."
    new_name <- getLine
    putStrLn "Insert birthday"
    new_birthday <- getLine     
    putStrLn "Insert speciality."
    new_speciality <- getLine 
    let person = Person {
        lastName = new_name, 
        birthday = new_birthday, 
        speciality = new_speciality
    }
    let newList = list ++ [person]
    putStrLn ("New List: " ++ show(newList))
    return newList 


editPerson :: [Person] -> IO [Person]
editPerson list = do
        if (length list) == 0 then do
                putStrLn "\nEmpty list"
                return list
                else do
        putStrLn "\nEnter the index of element to be changed"
        number <- enterInRange 1 (length list)
        putStrLn "Select field to be modified: "
        putStrLn "1. Name"
        putStrLn "2. Birthday"
        putStrLn "3. Speciality"
        field <- enterInRange 1 3      
        case field of
                1 -> putStrLn "Choose a new name"
                2 -> putStrLn "Choose a new birthday"
                3 -> putStrLn "Choose a new speciality"
        edit_field <- getLine
        let elem = list !! (number - 1)
        let part_left = take (number - 1) list
        let part_right = drop number list
        let part_centr = case field of 
                        1 -> Person {
                                lastName = edit_field, 
                                birthday = birthday elem, 
                                speciality = speciality elem
                        }
                        2 -> Person {
                                lastName = lastName elem, 
                                birthday = edit_field, 
                                speciality = speciality elem
                        }
                        3 -> Person {
                                lastName = lastName elem , 
                                birthday = birthday elem, 
                                speciality = edit_field :: String
                        }
        let newList = part_left ++ [part_centr] ++ part_right
        putStrLn ("Edited list: " ++ show newList)
        return newList

deletePerson :: [Person] -> IO [Person]
deletePerson list = do
    if (length list) == 0 then do
            putStrLn "\nEmpty List"
            return list
    else do
        putStrLn "\nEnter the index of element to be deleted"
        number <- enterInRange 1 (length list)
        let newList = (take (number -1) list) ++ (drop number list)
        putStrLn ("New list: " ++ show newList)
        return newList

printToFile :: [Person] -> IO [Person]
printToFile list = do 
        let path = "file.txt"
        let array = map (\p -> (lastName p, birthday p, speciality p)) list 
        writeFile path (show array)
        putStrLn "\nPerson printed into the file."
        return list

convertToPersonList :: [(String, String, String)] -> [Person]
convertToPersonList [] = []
convertToPersonList ((name, bday, spec):rest) = 
    let person = Person {
            lastName = name,
            birthday = bday,
            speciality = spec 
        }
    in person : convertToPersonList rest

readFromFile :: [Person] -> IO [Person]
readFromFile list = do
        str_output <- readFile "file.txt" 
        let array = (read str_output) :: [([Char], [Char], String)]
        let newList = convertToPersonList array
        putStrLn ("List inserted: " ++ show newList) 
        return newList

select :: [Person] -> (Person -> Bool) -> [Person]
select [] _ = []
select (p:ps) f = if f p then p : rest else rest 
    where 
        rest = select ps f 

getWithBirthday :: [Person] -> IO [Person]
getWithBirthday ps = do
    putStrLn "\nEnter birthday"
    date <- getLine
    let people = select ps (\p -> birthday p == date)
    putStrLn $ "People born in " ++ date ++ ": " ++ show people
    return ps 

mostCommonSurname :: [Person] -> IO [Person]
mostCommonSurname ps = do 
    let mostCommon = map fst $ getMax (map (\x -> (head x, length x)) $ group $ sort names)
    putStrLn $ "Most common surnames: " ++ show mostCommon
    return ps 
    where 
        names = map lastName ps
        getMax [] = [] 
        getMax (x:xs) = 
            case (getMax xs) of
                [] -> [x]
                (y:ys) -> if snd x < snd y then y:ys else 
                            if snd x == snd y then x:y:ys else [x]

exit :: [Person] -> IO [Person]
exit list = do 
  let newList = []
  return newList 