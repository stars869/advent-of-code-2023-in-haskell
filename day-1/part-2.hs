import Data.List (find, isPrefixOf, tails)
import Data.Map (Map, fromList, keys, (!))
import Data.Maybe (catMaybes)
import Control.Applicative (Applicative(liftA2))


numsDict :: Map String Int 
numsDict = fromList $ zip 
    ["one","two","three","four","five","six","seven","eight","nine", "1","2","3","4","5","6","7","8","9"] 
    (cycle [1..9])

solveCase :: String -> Int
solveCase = liftA2 (+) ((*10). head) last . parseNums
    where 
       parseNums =  map (numsDict !) . catMaybes . map suffixMatch . tails
       suffixMatch str = find (\k -> isPrefixOf k str) $ keys numsDict

solve :: [String] -> Int 
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . lines
    