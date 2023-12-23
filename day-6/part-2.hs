import System.IO
import MyHelpers (splitOn)
import Debug.Trace (trace)

calcAllDistancesByTime :: Int -> [Int]
calcAllDistancesByTime t = map (\x -> x * (t - x)) [0..t]

numberOfWaysToBeat :: (Int, Int) -> Int
numberOfWaysToBeat (time, distance) = length $ filter (> distance) (calcAllDistancesByTime time)

solve :: [String] -> Int
solve input = numberOfWaysToBeat (time, distance)  
    where
        time = read $ filter (/=' ') $ last $ splitOn (==':') $ head input :: Int
        distance = read $ filter (/=' ') $ last $ splitOn (==':') $ last input :: Int

main :: IO ()
main = do
    input <- readFile "./input"
    let result = solve $ lines input
    print result