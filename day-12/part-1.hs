import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import MyHelpers (splitOn)


parse :: String -> [(String, [Int])]
parse = map ((head &&& (parseList . last)) . words) . lines
    where
        parseList = map read . splitOn (==',')

bruteForce :: String -> [String]
bruteForce [] = [[]]
bruteForce (x:xs) 
    | x == '?' = liftA2 (++) (map ('.':)) (map ('#':)) (bruteForce xs)
    | otherwise = map (x:) (bruteForce xs)

solveCase :: (String, [Int]) -> Int 
solveCase (springs, record) = length $ filter (==record) $ map (map length . splitOn (=='.')) $ bruteForce springs

solve :: [(String, [Int])] -> Int
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse

