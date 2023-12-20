import System.IO
import MyHelpers (padding2D, conv2D)
import Data.List (groupBy, sortBy)
import System.Console.Terminfo (restoreDefaultColors)
import Data.Set (toList, fromList)
import Data.Char (isDigit)
import Debug.Trace (trace)


type GearID = Int
type CharWithGearIDs = (Char, [GearID])
type NumWithGearIDs = (Int, [GearID])
type GearIDWithNums = (GearID, [Int])

generateGearID :: GearID -> [Char] -> [GearID]
generateGearID _ [] = []
generateGearID gearID (x:xs) =
    if x == '*' then gearID: generateGearID (gearID+1) xs
    else 0: generateGearID (gearID+1) xs

generateGearID2D :: GearID -> [[Char]] -> [[GearID]]
generateGearID2D gearID (y:ys) = generateGearID gearID y: generateGearID2D (gearID + length y) ys

kernalGetGears :: [[(Char, GearID)]] -> [GearID]
kernalGetGears = filter (> 0) . map snd . concat

calculateGearRatioSum :: [GearIDWithNums] -> Int
calculateGearRatioSum gearIDWithNums = sum $ map (\e -> (head $ snd e) * (last $ snd e)) gearIDWithTwoNums
    where gearIDWithTwoNums = filter (\e -> (length $ snd e) == 2) gearIDWithNums

getGearIDWithNums :: [NumWithGearIDs] -> [GearIDWithNums]
getGearIDWithNums list = map (\e -> (fst $ head e, map snd e)) groupedGears
    where
        flattenedList = concat $ map (\e -> map (\g -> (g, fst e)) $ snd e) list
        groupedGears = groupBy (\x y -> fst x == fst y) $ sortBy (\x y -> compare (fst x) (fst y)) flattenedList

rmdups :: (Ord a) => [a] -> [a]
rmdups = toList . fromList

takeDigits :: [CharWithGearIDs] -> [CharWithGearIDs]
takeDigits = takeWhile (\e -> isDigit (fst e))

dropDigits :: [CharWithGearIDs] -> [CharWithGearIDs]
dropDigits = dropWhile (\e -> isDigit (fst e))

dropNotDigits :: [CharWithGearIDs] -> [CharWithGearIDs]
dropNotDigits = dropWhile (\e -> not $ isDigit (fst e))

readWithGearID :: [CharWithGearIDs] -> (Int, [GearID])
readWithGearID [] = (0, [])
readWithGearID list = if length gearIDs > 0
    then (read (map fst list), gearIDs)
    else (0, [])
    where
        gearIDs = rmdups $ concat $ map snd list

readNums :: [CharWithGearIDs] -> [NumWithGearIDs]
readNums [] = []
readNums list = readWithGearID (takeDigits list) : readNums (dropNotDigits (dropDigits list))

readNumsMultiRows :: [[CharWithGearIDs]] -> [NumWithGearIDs]
readNumsMultiRows mat = concat $ map (\r -> readNums r) mat

solve :: [[Char]] -> Int
solve mat = calculateGearRatioSum gearIDWithNums
    where
        gearIDWithNums = getGearIDWithNums numWithGearIDs
        numWithGearIDs = readNumsMultiRows charWithGearIDs
        charWithGearIDs = zipWith zip mat allGearIDsInACell
        allGearIDsInACell = conv2D 3 kernalGetGears (padding2D 1 ('.', 0) charWithGearID)
        charWithGearID = zipWith zip mat gearIDs
        gearIDs = generateGearID2D 0 mat

main :: IO ()
main = do
    input <-  readFile "./input"
    let result = solve $ lines input
    print result
