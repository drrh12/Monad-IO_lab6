import Data.List

data Person = Person {
    lastName :: String,
    birthDay :: String,
    speciality :: String
} 

instance Show Person where
    show (Person lastName date spec) = " " ++ lastName ++ " (" ++ date ++ "): " ++ spec ++ " "

addPerson :: [Person] -> String -> String -> String -> [Person]
addPerson list lastName date spec = list ++ [Person lastName date spec]

editPerson :: [Person] -> Int -> String -> String -> String -> [Person]
editPerson list index lastName date spec
    | index <= 0 || length list < index = list
    | index == 1 = Person lastName date spec : tail list
    | otherwise  = head list : editPerson (tail list) (index-1) lastName date spec 

deletePerson :: [Person] -> Int -> [Person]
deletePerson list index 
    | index <= 0 || length list < index = list
    | index == 1 = tail list
    | otherwise  = head list : deletePerson (tail list) (index-1)

select :: [Person] -> (Person -> Bool) -> [Person]
select [] _ = []
select (p:ps) f = if f p then p : rest else rest 
    where 
        rest = select ps f 

getWithBirthday :: [Person] -> String -> [Person]
getWithBirthday ps date = select ps (\p -> birthDay p == date)

mostCommonSurname :: [Person] -> [String]
mostCommonSurname ps = map fst $ getMax (map (\x -> (head x, length x)) $ group $ sort names)
    where 
        names = map lastName ps
        getMax [] = [] 
        getMax (x:xs) = 
            case (getMax xs) of
                [] -> [x]
                (y:ys) -> if snd x < snd y then y:ys else 
                            if snd x == snd y then x:y:ys else [x]

person1 = Person "Smith" "01/01/1998" "Music"
person2 = Person "Doe" "02/05/1994" "Cooking"
person3 = Person "Smith" "04/09/1989" "Music"
person4 = Person "Charles" "01/01/1998" "Hockey"
list = [person1, person2, person3, person4]

main = do 
    print list 
    putStrLn ""
    let newList = addPerson list "Williams" "03/03/1993" "Surf"
    print newList
    putStrLn ""
    let newList' = editPerson newList 5 "Jones" "01/01/1998" "Surf"
    print newList'
    putStrLn ""
    let newList'' = deletePerson newList' 2 
    print newList''
    putStrLn ""
    print $ getWithBirthday newList'' "01/01/1998"
    putStrLn ""
    print $ mostCommonSurname newList''