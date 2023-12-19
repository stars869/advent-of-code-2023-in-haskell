import System.IO
import Data.Char (isDigit, digitToInt, ord)
import MyHelpers (padding2D, conv2D)


type CharWithMark = (Char, Bool)

readWithMark :: [CharWithMark] -> Int
readWithMark [] = 0
readWithMark list = if any snd list
    then read (map fst list)
    else 0

takeDigits :: [CharWithMark] -> [CharWithMark]
takeDigits = takeWhile (\e -> isDigit (fst e))

dropDigits :: [CharWithMark] -> [CharWithMark]
dropDigits = dropWhile (\e -> isDigit (fst e))

dropNotDigits :: [CharWithMark] -> [CharWithMark]
dropNotDigits = dropWhile (\e -> not $ isDigit (fst e))

readNums :: [CharWithMark] -> [Int]
readNums [] = []
readNums list = readWithMark (takeDigits list) : readNums (dropNotDigits (dropDigits list))

isSymbol :: Char -> Bool
isSymbol c = ord c < ord '.' || ord c > ord '9' || ord c == ord '/' 

kernalHasSymbol :: [[Char]] -> Bool
kernalHasSymbol = any (any isSymbol) 

solve :: [[Char]] -> Int
solve mat = sum $ map (\r -> sum $ readNums r) charWithMarks
    where 
        marks = conv2D 3 kernalHasSymbol (padding2D 1 '.' mat)
        charWithMarks = zipWith zip mat marks 

main :: IO ()
main = do
    input <-  readFile "./input"
    let result = solve $ lines input
    print result
