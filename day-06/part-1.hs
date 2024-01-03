import Control.Applicative (liftA2)
import MyHelpers (splitOn)


parse :: String -> [(Int, Int)]
parse = liftA2 zip (parseLine . head) (parseLine . last) . lines
    where parseLine = map read . tail . words

nWays :: (Int, Int) -> Int
nWays (time, distance) = length $ filter (> distance) $ map (\x -> x * (time - x)) [0..time]

solve :: [(Int, Int)] -> Int
solve = product . map nWays 

main :: IO ()
main = do
    readFile "./input" >>= print . solve . parse