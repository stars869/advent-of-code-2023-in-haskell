import System.IO
import Debug.Trace (trace)
import MyHelpers (splitOn)
import qualified MyRange
import Data.Maybe (catMaybes)

type Range = (Int, Int)
type RangeShift = (Range, Int)

splitEveryTwo :: [a] -> [(a, a)]
splitEveryTwo [] = []
splitEveryTwo (x:y:rest) = (x, y) : splitEveryTwo rest
splitEveryTwo _ = error "Incomplete pair"

readSeedRanges :: String -> [Range]
readSeedRanges lineStr = map (\p -> (fst p, fst p + snd p - 1)) $ splitEveryTwo nums
    where
        nums = map read $ tail $ splitOn (==' ') lineStr

readSingleShift :: String -> RangeShift
readSingleShift lineStr = (sourceRange, shiftLength)
    where
        sourceRange = (sourceStart, sourceStart + rangeLength - 1)
        shiftLength = destStart - sourceStart
        destStart = nums !! 0
        sourceStart = nums !! 1
        rangeLength = nums !! 2
        nums = map read $ splitOn (==' ') lineStr :: [Int]

readWholePermutation :: [String] -> [RangeShift]
readWholePermutation linesStr = map readSingleShift $ tail linesStr

-- TODO: to be implemented
applyPermute :: [RangeShift] -> Range -> [Range]
applyPermute rangeShiftList inputRange = nonIntersections ++ shiftedIntesections
    where
        nonIntersections = MyRange.substractDomain [inputRange] (map fst rangeShiftList) :: [Range]
        shiftedIntesections = catMaybes $ map (\rs -> applyRangeShiftOnIntesection rs inputRange) rangeShiftList
        applyRangeShiftOnIntesection (sourceRange, shiftLength) range = case intersection of 
            Nothing -> Nothing 
            Just value -> Just $ MyRange.shift shiftLength value  
            where intersection = MyRange.intersect sourceRange inputRange


applyPermteToRangeList :: [RangeShift] -> [Range] -> [Range]
applyPermteToRangeList permuteList = concatMap (applyPermute permuteList) 

solve :: [String] -> Int
solve linesStr = minimum $ map fst locationRanges
    where
        locationRanges = foldl (\r p -> p r) seedRanges permuteList :: [Range]
        seedRanges = readSeedRanges $ head $ head strChunks :: [Range]
        permuteList = map applyPermteToRangeList permutationList :: [[Range] -> [Range]]
        permutationList = map readWholePermutation $ tail strChunks :: [[RangeShift]]
        strChunks = splitOn (=="") linesStr :: [[String]]

main :: IO ()
main = do
    input <- readFile "./input"
    let result = solve $ lines input
    print result
