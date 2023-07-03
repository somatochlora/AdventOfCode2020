import System.Environment
import Data.List (elemIndices)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let grid = map parseLine $ lines text
        let path = checkDiag (3, 1) (0, 0) grid
        print $ countTrue path

        let rules = [(1,1),(3,1),(5,1),(7,1),(1,2)]
        let paths = [checkDiag x (0, 0) grid | x <- rules]
        let counts = map countTrue paths
        print $ product counts

checkDiag :: (Int, Int) -> (Int, Int) -> [[Bool]] -> [Bool]
checkDiag (right, down) (startX, startY) grid
    | startY >= (height grid) = []
    | otherwise = (getGridSquare (startX, startY) grid) : rest
    where   newCoords = (((startX + right) `mod` (width grid)), (startY+down))
            rest = checkDiag (right, down) newCoords grid

getGridSquare :: (Int, Int) -> [[a]] -> a
getGridSquare (x, y) grid = grid !! y !! x

width :: [[a]] -> Int
width = length . head

height :: [[a]] -> Int
height = length

parseLine :: String -> [Bool]
parseLine = map f
    where   f '#' = True
            f '.' = False

countTrue :: [Bool] -> Int
countTrue = foldl (\acc x -> case x of { True -> acc + 1 ; False -> acc}) 0


