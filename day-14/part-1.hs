import Data.List (transpose, sort)
import Debug.Trace (trace)


splitByCubes :: [Char] -> [[Char]]
splitByCubes [] = []
splitByCubes ('#':rocks) = ['#']: splitByCubes rocks
splitByCubes rocks = takeWhile (/='#') rocks : splitByCubes (dropWhile (/='#') rocks)

solveRow :: [Char] -> Int
solveRow rocks = sum $ zipWith (*) (map (\c -> if c=='O' then 1 else 0) $ sortRocks rocks) (reverse [1..(length rocks)])
    where 
        sortRocks = concat . map (reverse . sort) . splitByCubes

solve :: [[Char]] -> Int
solve rocks = sum $ map solveRow (transpose rocks)

main :: IO ()
main = readFile "./input" >>= print . solve . lines