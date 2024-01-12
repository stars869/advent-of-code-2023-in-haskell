import MyHelpers (splitOn)
import Data.Char (ord)


parse :: String -> [String]
parse = splitOn (==',')

solveCase :: String -> Int
solveCase = foldl (\value c -> (value + ord c) * 17 `mod` 256) 0

solve :: [String] -> Int
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse