import Control.Arrow ((&&&))
import Data.List (intersect)
import Data.Array (listArray, elems, (!))
import MyHelpers (splitOn)


parse :: String -> [([Int], [Int])]
parse = map parseLine . lines
    where
        parseLine = (toNums . head &&& toNums . last) . splitOn (=='|') . last . splitOn (==':')
        toNums = map (read @Int) . words

solve :: [([Int], [Int])] -> Int
solve cards = sum $ elems memo
    where
        nMatchesList = map (length . uncurry intersect) cards
        nMatchesArray = listArray (1, length nMatchesList) nMatchesList
        memo = listArray (1, length nMatchesList) [winningCopies i | i <- [1..(length nMatchesList)]]
        winningCopies i = 1 + sum (map (\di -> memo ! (i + di)) [1..(nMatchesArray ! i)])

main :: IO ()
main = readFile "./input" >>= print . solve . parse