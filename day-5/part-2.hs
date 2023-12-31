import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import Data.Either (rights, lefts)
import MyHelpers (splitOn, takeEveryTwo)


type Range = (Int, Int)
type RangeWithMark = Either Range Range

parse :: String -> ([Range], [[(Int, Int, Int)]])
parse = (parseSeeds &&& parseMappings) . splitOn (=="") . lines
    where
        parseSeeds = map toRange . takeEveryTwo . map read . tail . words . head . head
        toRange (s, l) = (s, s+l-1)
        parseMappings = map parseMapping . tail
        parseMapping = map (toTuple . map read . words) . tail
        toTuple [n1, n2, n3] = (n1, n2, n3)

mapRangesPartial :: [RangeWithMark] -> (Int, Int, Int) -> [RangeWithMark] 
mapRangesPartial ranges (dest, start, len) = concatMap applyShift ranges
    where 
        (end, shift) = (start + len - 1, dest - start)
        applyShift (Right range) = [Right range] 
        applyShift (Left (l, r)) 
            | r < start = [Left (l, r)]
            | l > end = [Left (l, r)]
            | l >= start && r <= end = [Right (l+shift, r+shift)]
            | l < start && r >= start && r <= end = [Left (l, start-1), Right (start+shift, r+shift)]
            | l >= start && l <= end && r > end = [Right (l+shift, end+shift), Left (end+1, r)]
            | l < start && r > end = [Left (l, start-1), Right (start+shift, end+shift), Left (end+1, r)]

mapRanges :: [Range] -> [(Int, Int, Int)] -> [Range]
mapRanges ranges mapping = liftA2 (++) lefts rights $ foldl mapRangesPartial (map Left ranges) mapping 

solve :: ([Range], [[(Int, Int, Int)]]) -> Int
solve (seedRanges, mappings) = minimum $ map fst $ foldl mapRanges seedRanges mappings 

main :: IO ()
main = readFile "./input" >>= print . solve . parse