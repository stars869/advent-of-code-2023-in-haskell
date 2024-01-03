import Data.Map (Map, fromList, (!), keys)
import Control.Arrow ((&&&))
import Data.Char (isAlpha)
import Debug.Trace (trace)


parse :: String -> (String, Map String (String, String))
parse = head . lines &&& toMap . tail . tail . lines
    where
        toMap = fromList . map (toPair . split)
        split = words . filter (\c -> isAlpha c || (c==' '))
        toPair [start, left, right] = (start, (left, right))

solve :: (String, Map String (String, String)) -> Int
solve (instruction, network) = foldl lcm 1 $ map (length . findPath) (getAllStart network) 
    where
        findPath start = takeWhile (not . isEnd) $ scanl move start (cycle instruction)
        move start choice = (lOrR choice . (network !)) start
        lOrR choice = if choice == 'L' then fst else snd
        getAllStart = filter (\k -> last k == 'A') . keys 
        isEnd = (== 'Z') . last

main :: IO ()
main = readFile "./input" >>= print . solve . parse