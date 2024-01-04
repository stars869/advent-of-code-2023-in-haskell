import Data.Map (Map, fromList, (!))
import Debug.Trace (trace)
import Control.Applicative (Applicative(liftA2))
import qualified Data.Set as Set  

type Coord = (Int, Int)

addCoord :: Coord -> Coord -> Coord
addCoord (x, y) (x', y') = (x + x', y + y')

tileMap :: Map Char [(Int, Int)]
tileMap = fromList [
        ('F', [(0, 1),(1, 0)]),
        ('-', [(0, 1),(0, -1)]),
        ('7', [(0, -1),(1, 0)]),
        ('|', [(1, 0),(-1, 0)]),
        ('J', [(-1, 0),(0, -1)]),
        ('L', [(-1, 0),(0, 1)]),
        ('S', [(1, 0),(0, 1),(-1,0),(0,-1)]),
        ('.', [])
    ]

parse :: String -> [[(Coord, [Coord])]]
parse input = zipWith zip coordMat tileMat'
    where
        charMat = lines input
        (nRows, nCols) = (length charMat, length $ head charMat)
        coordMat = map (\ri -> map (ri,) [1..nCols]) [1..nRows]
        tileMat = map (map (tileMap !)) charMat
        tileMat' = zipWith (zipWith (map . addCoord)) coordMat tileMat

type State = (Coord, Coord)

move :: Map Coord [Coord] -> State -> State
move pipeMap (preCoord, currentCoord) = (currentCoord, nextCoord)
    where 
        pipeDests = pipeMap ! currentCoord 
        nextCoord = head $ filter (/=preCoord) pipeDests

getPipePath :: [[(Coord, [Coord])]] -> [Coord]
getPipePath tileMat = liftA2 (++) (map fst) ((:[]) . snd . last) $ takeWhile (not . (==(fst start)) . snd) $ iterate moveWithMap currentState
    where 
        pipeMap = fromList $ concat tileMat
        moveWithMap = move pipeMap
        start = head $ filter ((==4) . length . snd) $ concat tileMat
        nextCoord = head $ filter (\coord -> elem (fst start) (pipeMap ! coord)) (snd start)
        currentState = (fst start, nextCoord)

shoelace pts = abs . sum $ zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - y1 * x2) pts (drop 1 pts ++ pts)

solve :: [[(Coord, [Coord])]] -> Int 
solve pipeMat = ((shoelace pipePath) - (length pipePath)) `div` 2 + 1 
    where 
        pipePath = getPipePath pipeMat

main :: IO ()
main = readFile "./input" >>= print . solve . parse