import qualified Data.Set as Set
import Data.Char (isDigit)
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
solve schematic = sum $ map (read @Int . snd) partNums
    where
        isSymbol c = not (isDigit c) && (c /= '.')
        partCoords = Set.fromList $ map fst $ filter (isSymbol . snd) $ concat schematic
        nums = map unzip $ concatMap (splitOn (not. isDigit . snd)) schematic
        partNums = filter (\(cords, _) -> any (any (`Set.member` partCoords) . neighbors) cords) nums

main :: IO ()
main = readFile "./input" >>= print . solve . parse