import System.IO
import MyHelpers (padding2D, conv2D, splitOn)
import Data.List (groupBy, sortBy, sort)
import Data.Char (isDigit)
import Debug.Trace (trace)


type GearID = Int
type PartNum = Int

getGearIDs :: [[Char]] -> [[GearID]]
getGearIDs mat = zipWith (zipWith (\c i -> if c == '*' then i else 0)) mat indexMat
    where
        indexMat = map (\ri -> map (\ci -> ri * nCols + ci) [1..nCols]) [1..nRows] :: [[Int]]
        nRows = length mat
        nCols = length $ head mat

kernalGetGears :: [[(Char, GearID)]] -> [GearID]
kernalGetGears = filter (> 0) . map snd . concat

calculateGearRatioSum :: [(GearID, [PartNum])] -> Int
calculateGearRatioSum gearIDWithNums = sum $ map (\e -> (head $ snd e) * (last $ snd e)) gearIDWithTwoNums
    where gearIDWithTwoNums = filter (\e -> (length $ snd e) == 2) gearIDWithNums

getGearIDWithNums :: [(PartNum, [GearID])] -> [(GearID, [PartNum])]
getGearIDWithNums list = map (\e -> (fst $ head e, map snd e)) groupedGears
    where
        flattenedList = concatMap (\e -> map (\g -> (g, fst e)) $ snd e) list
        groupedGears = groupBy (\x y -> fst x == fst y) $ sortBy (\x y -> compare (fst x) (fst y)) flattenedList

readWithGearID :: [(Char, [GearID])] -> (PartNum, [GearID])
readWithGearID [] = (0, [])
readWithGearID list = if length gearIDs > 0
    then (read (map fst list), gearIDs)
    else (0, [])
    where
        gearIDs = map head $ groupBy (==) $ sort $ concatMap snd list

readNums :: [(Char, [GearID])] -> [(PartNum, [GearID])]
readNums list = map readWithGearID $ splitOn (not . isDigit . fst) list

solve :: [[Char]] -> Int
solve mat = calculateGearRatioSum gearIDWithNums
    where
        gearIDWithNums = getGearIDWithNums numWithGearIDs
        numWithGearIDs = concatMap readNums charWithGearIDs
        charWithGearIDs = zipWith zip mat allGearIDsInACell
        allGearIDsInACell = conv2D 3 kernalGetGears (padding2D 1 ('.', 0) charWithGearID)
        charWithGearID = zipWith zip mat gearIDs
        gearIDs = getGearIDs mat

main :: IO ()
main = do
    input <-  readFile "./input"
    let result = solve $ lines input
    print result
