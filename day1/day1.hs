import System.Environment

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let nums = map read $ lines text
        let product = pairProduct <$> findSumPair nums 2020
        print product
        let product2 = tripProduct <$> findSumTriplet nums 2020
        print product2

pairProduct :: (Int, Int) -> Int
pairProduct (x, y) = x * y

tripProduct :: (Int, Int, Int) -> Int
tripProduct (x, y, z) = x * y * z

findSumPair :: [Int] -> Int -> Maybe (Int, Int)
findSumPair ls n
    | pairs == [] = Nothing
    | otherwise = Just $ head pairs
    where pairs = findSumPairs ls n

findSumPairs :: [Int] -> Int -> [(Int, Int)]
findSumPairs ls n = [(a, b) | a <- ls, b <- ls, a + b == n, a /= b]

findSumTriplet :: [Int] -> Int -> Maybe (Int, Int, Int)
findSumTriplet ls n
    | trips == [] = Nothing
    | otherwise = Just $ head trips
    where trips = findSumTriplets ls n

findSumTriplets :: [Int] -> Int -> [(Int, Int, Int)]
findSumTriplets ls n = [(a, b, c) | a <- ls, b <- ls, c <- ls, a + b + c == n, a /= b, b /= c, a /= c]