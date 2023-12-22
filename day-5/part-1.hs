import System.IO
import Debug.Trace (trace)
import MyHelpers (splitOn)


readSeeds :: String -> [Int]
readSeeds lineStr = map read $ tail $ splitOn (==' ') lineStr

readSingleMapping :: String -> (Int -> Int)
readSingleMapping lineStr = mappingFunc 
    where 
        destStart = nums !! 0
        sourceStart = nums !! 1 
        range = nums !! 2
        nums = map read $ splitOn (==' ') lineStr :: [Int]
        mappingFunc source = if sourceStart <= source && source < sourceStart + range then source - sourceStart + destStart else -1 

readMappings :: [String] -> [(Int -> Int)]
readMappings linesStr = map readSingleMapping $ tail linesStr

applyMappings :: Int -> [(Int -> Int)] -> Int 
applyMappings source mappings = if null validDests then source else head validDests
    where 
        validDests = filter (>= 0) destinations
        destinations = map (\m -> m source) mappings

applyMappingChain :: Int -> [[(Int -> Int)]] -> Int 
applyMappingChain source mappingsList = foldl applyMappings source mappingsList 

solve :: [String] -> Int 
solve linesStr = minimum locations
    where
        locations = map (\s -> applyMappingChain s mappings) seeds 
        seeds = readSeeds $ head $ head strChunks :: [Int]
        mappings = map readMappings $ tail strChunks
        strChunks = splitOn (=="") linesStr :: [[String]]

main :: IO ()
main = do 
    input <- readFile "./input"
    let result = solve $ lines input 
    print result 
