import System.Environment
import Data.Char (isDigit)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let passports = map parsePassport $ splitBy "\n\n" text
        let valids = map isValid passports
        let count = length $ filter (==True) valids 
        print count
        let valids = map isValid' passports
        let count = length $ filter (==True) valids 
        print count

required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
required' = [("byr", birthValid),
    ("iyr", issueValid),
    ("eyr", expirationValid),
    ("hgt", heightValid),
    ("hcl", hairValid),
    ("ecl", eyeValid),
    ("pid", idValid),
    ("cid", (\a -> True))]

isValid' :: [(String, String)] -> Bool
isValid' xs
    | not $ isValid xs = False
    | otherwise = isValid'' xs

isValid'' :: [(String, String)] -> Bool
isValid'' [] = True
isValid'' ((k,v):xs)
    | not $ hasKey required' k = False
    | not $ f v = False
    | otherwise = isValid'' xs
    where f = getVal k required'

getVal :: String -> [(String, a)] -> a
getVal k' ((k, v):ms)
    | k' == k = v
    | otherwise = getVal k' ms

birthValid :: String -> Bool
birthValid s
    | length s /= 4 = False
    | n >= 1920 && n <= 2002 = True
    | otherwise = False
    where n = read s

issueValid :: String -> Bool
issueValid s
    | length s /= 4 = False
    | n >= 2010 && n <= 2020 = True
    | otherwise = False
    where n = read s

expirationValid :: String -> Bool
expirationValid s
    | length s /= 4 = False
    | n >= 2020 && n <= 2030 = True
    | otherwise = False
    where n = read s

heightValid :: String -> Bool
heightValid s
    | unit /= "cm" && unit /= "in" = False
    | (val < 150 || val > 193) && unit == "cm" = False
    | (val < 59 || val > 76) && unit == "in" = False
    | otherwise = True
    where   unit = lastN 2 s
            val = read $ take (length s - 2) s

hairValid :: String -> Bool
hairValid (x:xs)
    | x /= '#' = False
    | length xs /= 6 = False
    | otherwise = and $ map rightDigit xs        

rightDigit :: Char -> Bool
rightDigit c
    | c >= '0' && c <= '9' = True
    | c >= 'a' && c <= 'f' = True
    | otherwise = False

eyeValid :: String -> Bool
eyeValid s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

idValid :: String -> Bool
idValid s
    | length s /= 9 = False
    | otherwise = and $ map isDigit s

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

isValid :: [(String, String)] -> Bool
isValid xs = and $ map (hasKey xs) required

hasKey :: [(String, a)] -> String -> Bool
hasKey [] _ = False
hasKey ((k,v):xs) k'
    | k' == k = True
    | otherwise = hasKey xs k'

parsePassport :: String -> [(String, String)]
parsePassport s = map toKeyVal bits
    where bits = concat $ map words $ lines s

toKeyVal :: String -> (String, String)
toKeyVal [] = ("","")
toKeyVal s
    | length bits <= 1 = (s,"")
    | otherwise = (bits!!0, bits!!1)
    where bits = splitBy ":" s 

splitBy :: (Eq a) => [a] -> [a] -> [[a]]
splitBy del lis
    | length lis < n = [lis]
    | otherwise = case pos of
        Nothing -> [lis]
        Just x -> (take x lis):(splitBy del (drop (x + length del) lis))
    where   pos = findMatch del lis
            n = length lis 

findMatch :: (Eq a) => [a] -> [a] -> Maybe Int
findMatch _ [] = Nothing
findMatch match lis@(x:xs)
    | length lis < n = Nothing
    | take n lis == match = Just 0
    | otherwise = (+1) <$> findMatch match xs 
    where n = length match