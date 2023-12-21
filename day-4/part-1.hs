import qualified Data.Set as Set
import MyHelpers (splitOn)
import Distribution.Utils.String (trim)
import Debug.Trace (trace)

solveCase :: String -> Int
solveCase card = if power == 0 then 0 else 2 ^ (power - 1)
    where 
        power = length $ filter (\n -> Set.member n winingNums) myNums
        winingNums = Set.fromList $ map read $ splitOn (==' ') $ trim $ last $ splitOn (==':') $ head $ splitOn (=='|') card 
        myNums = map read $ splitOn (==' ') $ trim $ last $ splitOn (=='|') card :: [Int]


main :: IO ()
main = do 
    input <- readFile "./input"
    let result = sum $ map solveCase $ lines input 
    print result 