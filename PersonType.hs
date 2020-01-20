module PersonType where

data Person = Person {
    lastName :: [Char],
    birthday :: [Char],
    speciality :: String
} deriving (Eq, Ord)

instance Show Person where
    show (Person lastName date spec) = " " ++ lastName ++ " (" ++ date ++ "): " ++ spec ++ " "
