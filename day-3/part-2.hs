import qualified Data.Set as Set
import Data.Char (isDigit)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import MyHelpers (splitOn)


type Coord = (Int, Int)

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

parse :: String -> [[(Coord, Char)]]
parse str = zipWith zip cordMat charMat
    where
        charMat = lines str
        (nRows, nCols) = (length charMat, length $ head charMat)
        cordMat = map (\ri -> map (ri,) [1..nCols]) [1..nRows]

solve :: [[(Coord, Char)]] -> Int
solve schematic = sum $ map (product . map snd) gearWith2Nums
    where
        gearCoords = Set.fromList $ map fst $ filter ((=='*') . snd) $ concat schematic
        nums = map (\(coords, str) -> (coords, read @Int str)) $ map unzip $ concatMap (splitOn (not. isDigit . snd)) schematic
        numsWithGears = map (\(coords, num) -> (filter (`Set.member` gearCoords) $ Set.toList $ Set.fromList $ concatMap neighbors coords, num)) nums
        gearWithNums = groupBy (on (==) fst) $ sortBy (on compare fst) $ concatMap (\(coords, num) -> map (,num) coords) numsWithGears
        gearWith2Nums = filter ((==2) . length) gearWithNums 

main :: IO ()
main = readFile "./input" >>= print . solve . parse