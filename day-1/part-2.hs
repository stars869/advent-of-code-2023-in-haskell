import System.IO
import Data.List (find, isPrefixOf)
import Data.Char (isDigit, digitToInt)
import Data.Map (Map, fromList, keys, lookup, (!))
import Data.Maybe (catMaybes)

import Prelude hiding (lookup)

numsDict :: Map String Int 
numsDict = fromList 
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

parseNum :: String -> Maybe Int
parseNum inputStr 
    | isDigit $ head inputStr = Just $ digitToInt $ head inputStr 
    | otherwise = case matchedNum of 
        Just num -> lookup num numsDict  
        Nothing -> Nothing
    where 
        matchedNum = find (\k -> isPrefixOf k inputStr) $ keys numsDict 

parseNums :: String -> [Maybe Int]
parseNums [] = []
parseNums (x:tail) = parseNum (x:tail) : parseNums tail  


solveCase :: String -> Int
solveCase str = head nums * 10 + last nums
    where nums = catMaybes $ parseNums str

main :: IO ()
main = do
    input <-  readFile "./input"
    let result = sum $ map solveCase $ lines input
    print result
    