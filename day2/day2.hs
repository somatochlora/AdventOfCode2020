import System.Environment
import Data.List (elemIndices)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let passwords = map parseLine $ lines text
        let validCount = foldl (\acc p -> case (validatePassword p) of { True -> acc + 1 ; False -> acc}) 0 passwords
        print validCount
        let validCount2 = foldl (\acc p -> case (validatePassword' p) of { True -> acc + 1 ; False -> acc}) 0 passwords
        print validCount2

data RulePassword = RulePassword { num1 :: Int, num2 :: Int, character :: Char, password :: String} deriving (Show)

validatePassword :: RulePassword -> Bool
validatePassword p = (num1 p) <= count && (num2 p) >= count
    where count = length $ elemIndices (character p) (password p)

validatePassword' :: RulePassword -> Bool
validatePassword' p = (length (password p) >= (num2 p)) && (pos1 || pos2) && (not pos1 || not pos2)
    where   pos1 = (password p)!!(num1 p - 1) == (character p)
            pos2 = (password p)!!(num2 p - 1) == (character p)

parseLine :: String -> RulePassword
parseLine s = RulePassword {num1=counts!!0, num2=counts!!1, character = head $ bits!!1, password = bits!!2}
    where   bits = words s
            counts = map read $ halveOnChar '-' $ bits!!0

halveOnChar :: Char -> String -> [String]
halveOnChar c s = [take index s, drop (index+1) s]
    where index = head $ elemIndices c s
