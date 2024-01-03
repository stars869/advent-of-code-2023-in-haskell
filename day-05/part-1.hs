import Data.List (find)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import MyHelpers (splitOn)


parse :: String -> ([Int], [[(Int, Int, Int)]])
parse = (parseSeeds &&& parseMappings) . splitOn (=="") . lines
    where 
        parseSeeds = map read . tail . words . head . head
        parseMappings = map parseMapping . tail
        parseMapping = map (toTuple . map read . words) . tail
        toTuple [n1, n2, n3] = (n1, n2, n3) 

applyMapping :: Int -> [(Int, Int, Int)] -> Int 
applyMapping seed mapping = shift $ find inRange mapping
    where 
        inRange (dest, start, len) = start <= seed && seed < start + len
        shift (Just (dest, start, len)) = seed - start + dest 
        shift Nothing = seed

solve :: ([Int], [[(Int, Int, Int)]]) -> Int 
solve (seeds, mappings) = minimum $ map (\seed -> foldl applyMapping seed mappings) seeds

main :: IO ()
main = readFile "./input" >>= print . solve . parse