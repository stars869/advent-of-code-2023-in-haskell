import Data.List (transpose)


parse :: String -> [[((Int, Int), Int)]]
parse input = zipWith zip coordMat galaxyMat
    where
        galaxyMat = map (map (\s -> if s=='#' then 1 else 0)) $ lines input
        (nRows, nCols) = (length galaxyMat, length $ head galaxyMat)
        coordMat = map (\ri -> map (ri,) [1..nCols]) [1..nRows]

addCoord :: (Int, Int) -> (Int, Int) -> (Int, Int)
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

solve :: [[((Int, Int), Int)]] -> Int
solve galaxyMat = (`div` 2) $ sum $ map (uncurry distance) [(g, g') | g <- galaxies, g' <- galaxies, g /= g']
    where
        expansionRows = scanl1 (+) $ map ((*999999) . fromEnum . (==0) . sum) $ map (map snd) $ galaxyMat
        expansionCols = scanl1 (+) $ map ((*999999) . fromEnum . (==0) . sum) $ map (map snd) $ transpose galaxyMat
        expansionMat = [[(r, c) | c <- expansionCols] | r <- expansionRows]
        expandedGalaxyMat = zipWith (zipWith (\(galaxy, hasGalaxy) expan -> (addCoord galaxy expan, hasGalaxy))) galaxyMat expansionMat
        galaxies = map fst $ filter ((==1) . snd) $ concat expandedGalaxyMat

main :: IO ()
main = readFile "./input" >>= print . solve . parse