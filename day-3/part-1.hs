import System.IO
import Data.Char (isDigit, digitToInt, ord)
import MyHelpers (padding2D, conv2D, splitOn)


readWithMark :: [(Char, Bool)] -> Int
readWithMark [] = 0
readWithMark list = if any snd list
    then read (map fst list)
    else 0

readNums :: [(Char, Bool)] -> [Int]
readNums list = map readWithMark $ splitOn (not . isDigit . fst) list

isSymbol :: Char -> Bool
isSymbol c = ord c < ord '.' || ord c > ord '9' || ord c == ord '/'

kernalHasSymbol :: [[Char]] -> Bool
kernalHasSymbol = any (any isSymbol)

solve :: [[Char]] -> Int
solve mat = sum $ map (sum . readNums) charWithMarks
    where
        marks = conv2D 3 kernalHasSymbol (padding2D 1 '.' mat)
        charWithMarks = zipWith zip mat marks

main :: IO ()
main = do
    input <-  readFile "./input"
    let result = solve $ lines input
    print result
