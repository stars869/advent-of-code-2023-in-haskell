import Data.List (transpose)
import Control.Applicative (liftA2)
import MyHelpers (splitOn)


parse :: String -> [[String]]
parse = splitOn (=="") . lines

solveCase :: [String] -> Int
solveCase patterns = head $ (++) (map (*100) symmetricRowI) symmetricColI
    where
        (nRows, nCols) = (length patterns, length $ head patterns)
        symmetricAtRowI i = and . liftA2 (zipWith (==)) (reverse . take i) (drop i)
        symmetricAtColI i = symmetricAtRowI i . transpose
        symmetricRowI = [ri | ri <- [1..nRows-1], symmetricAtRowI ri patterns]
        symmetricColI = [ci | ci <- [1..nCols-1], symmetricAtColI ci patterns]

solve :: [[String]] -> Int
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse