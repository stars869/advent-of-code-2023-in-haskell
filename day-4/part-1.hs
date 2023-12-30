import Control.Arrow ((&&&))
import Data.List (intersect)
import MyHelpers (splitOn)


parse :: String -> [([Int], [Int])]
parse = map parseLine . lines
    where 
        parseLine = (toNums . head &&& toNums . last) . splitOn (=='|') . last . splitOn (==':')
        toNums = map (read @Int) . words

solveCase :: ([Int], [Int]) -> Int 
solveCase (winningNums, myNums) = if power == 0 then 0 else 2 ^ (power - 1)
    where power = length $ intersect myNums winningNums

solve :: [([Int], [Int])] -> Int 
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse