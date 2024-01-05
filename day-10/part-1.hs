import Data.Map (Map, fromList, (!))
import Control.Applicative (liftA2)


type Coord = (Int, Int)

parse :: String ->[[(Coord, [Coord])]]
parse input = zipWith zip coordMat tileMat
    where
        charMat = lines input
        (nRows, nCols) = (length charMat, length $ head charMat)
        coordMat = map (\ri -> map (ri,) [1..nCols]) [1..nRows]
        tileMat = zipWith (zipWith parseTile) charMat coordMat

        parseTile tile coord = case tile of
            'F' -> [right coord, down coord]
            '-' -> [right coord, left coord]
            '7' -> [left coord, down coord]
            '|' -> [up coord, down coord]
            'J' -> [up coord, left coord]
            'L' -> [up coord, right coord]
            'S' -> [left coord, right coord, up coord, down coord] -- out of index error when S is on the edge
            '.' -> []

        left (ri, ci) = (ri, ci-1)
        right (ri, ci) = (ri, ci+1)
        up (ri, ci) = (ri-1, ci)
        down (ri, ci) = (ri+1, ci)

type State = (Coord, Coord)

walk :: Map Coord [Coord] -> State -> State
walk pipeMap (preCoord, curCoord) = (curCoord, nextCoord)
    where
        nextCoord = head $ filter (/=preCoord) $ pipeMap ! curCoord

getPath :: [[(Coord, [Coord])]] -> [Coord]
getPath tileMat = liftA2 (++) (map fst) ((:[]). snd . last) $ takeWhile ((/= startCoord) . snd) $ iterate (walk pipeMap) currentState
    where
        pipeMap = fromList $ concat tileMat
        startCoord = fst $ head $ filter ((==4) . length . snd) $ concat tileMat
        nextCoord = head $ filter (\coord -> startCoord `elem` (pipeMap ! coord)) (pipeMap ! startCoord)
        currentState = (startCoord, nextCoord)

solve :: [[(Coord, [Coord])]] -> Int 
solve tileMat = ceiling $ (/ 2) $ fromIntegral $ length $ getPath tileMat

main :: IO ()
main = readFile "./input" >>= print . solve . parse