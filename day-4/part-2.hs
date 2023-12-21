import qualified Data.Set as Set
import MyHelpers (splitOn)
import Distribution.Utils.String (trim)
import Debug.Trace (trace)

countWinningCards :: String -> Int
countWinningCards card = length $ filter (\n -> Set.member n winingNums) myNums
    where 
        winingNums = Set.fromList $ map read $ splitOn (==' ') $ trim $ last $ splitOn (==':') $ head $ splitOn (=='|') card 
        myNums = map read $ splitOn (==' ') $ trim $ last $ splitOn (=='|') card :: [Int]

calcAccWinningNums :: [Int] -> [Int] -> [Int]
calcAccWinningNums [] _ = []
calcAccWinningNums revWinningNums accWinningNums = newAcc: (calcAccWinningNums (tail revWinningNums) (newAcc:accWinningNums)) 
    where 
        newAcc = 1 + (sum $ take (head revWinningNums) accWinningNums) :: Int 

solve :: [String] -> Int 
solve cases = sum $ calcAccWinningNums (reverse winningNums) []
    where winningNums = map countWinningCards cases

main :: IO ()
main = do 
    input <- readFile "./input"
    let result = solve $ lines input 
    print result 