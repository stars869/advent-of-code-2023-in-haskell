import Data.Map (Map, fromList, (!))
import Debug.Trace (trace)


type Coord = (Int, Int)
type Tile = [Coord]

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

move :: Map Coord Tile -> State -> State
move pipeMap (preCoord, currentCoord) = (currentCoord, nextCoord)
    where 
        pipeDests = pipeMap ! currentCoord 
        nextCoord = head $ filter (/=preCoord) pipeDests

solve :: [[(Coord, [Coord])]] -> Int
solve tileMat = ceiling $ (/ 2) $ fromIntegral $ length $ takeWhile (not . (==(fst start)) . snd) $ iterate moveWithMap currentState
    where 
        pipeMap = fromList $ concat tileMat
        moveWithMap = move pipeMap
        start = head $ filter ((==4) . length . snd) $ concat tileMat
        nextCoord = head $ filter (\coord -> elem (fst start) (pipeMap ! coord)) (snd start)
        currentState = (fst start, nextCoord)

main :: IO ()
main = readFile "./input" >>= print . solve . parse