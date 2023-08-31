import System.Environment
import Data.List (sort)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let ids = map (seatId . passDecode) $ lines text
        let highestId = maximum ids
        print highestId
        print $ findMissing ids

findMissing :: [Int] -> Int
findMissing xs = findMissing' $ sort xs

findMissing' :: [Int] -> Int
findMissing' (x:x':xs)
    | x' - x == 1 = findMissing' (x':xs)
    | otherwise = x' - 1

passDecode :: String -> (Int, Int)
passDecode s = (rowDecode $ take 7 s, colDecode $ drop 7 s)

seatId :: (Int, Int) -> Int
seatId (r, c) = r * 8 + c

rowDecode :: String -> Int
rowDecode [] = 0
rowDecode (c:cs)
    | c == 'F' = rowDecode cs
    | c == 'B' = 2 ^ (length cs) + rowDecode cs

colDecode :: String -> Int
colDecode [] = 0
colDecode (c:cs)
    | c == 'L' = colDecode cs
    | c == 'R' = 2 ^ (length cs) + colDecode cs

