
import System.Environment
import Data.List (nub, sort, group)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let groups = map getGroup $ splitBy "\n\n" text
        let counts = map (length . nub . fst) groups
        print $ sum counts
        let counts' = map countUniversalAnswers groups
        print $ sum counts'

freqs :: (Eq a, Ord a) => [a] -> [(a, Int)]
freqs ls = map (\x -> (head x, length x)) . group . sort $ ls

getGroup :: String -> (String, Int)
getGroup s = (concat lns, length lns)
    where lns = lines s

countUniversalAnswers :: (String, Int) -> Int
countUniversalAnswers (s, n) = length $ filter (\(_, count) -> count == n) (freqs s)

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