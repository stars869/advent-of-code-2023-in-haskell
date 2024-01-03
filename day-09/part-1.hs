solveCase :: [Int] -> Int
solveCase = sum . map last . takeWhile (not . all (==0)) . iterate diff
    where
        diff xs = zipWith (-) (tail xs) xs

solve :: [[Int]] -> Int
solve = sum . map solveCase

parse :: String -> [[Int]]
parse = map (map read . words) . lines

main :: IO ()
main = readFile "./input" >>= print . solve . parse