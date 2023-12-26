import Data.Char (isDigit, digitToInt)
import Control.Applicative (liftA2)


solveCase :: String -> Int
solveCase = calcValue . getNums
    where 
        getNums = map digitToInt . filter isDigit
        calcValue = liftA2 (+) ((*10) . head) last 

solve :: [String] -> Int 
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . lines