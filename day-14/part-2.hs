import Data.List (transpose, sort)
import Debug.Trace (trace)


splitByCubes :: [Char] -> [[Char]]
splitByCubes [] = []
splitByCubes ('#':rocks) = ['#']: splitByCubes rocks
splitByCubes rocks = takeWhile (/='#') rocks : splitByCubes (dropWhile (/='#') rocks)

northLoad :: [[Char]] -> Int
northLoad rocks = sum $ map loadOnRow (transpose rocks)
    where
        loadOnRow rocks = sum $ zipWith (*) (map (\c -> if c=='O' then 1 else 0) rocks) (reverse [1..(length rocks)])

spin :: [[Char]] -> [[Char]]
spin rocks = trace (show (northLoad rocks)) $ tiltEast $ tiltSouth $ tiltWest $ tiltNorth rocks
    where 
        rowTiltWest = concat . map (reverse . sort) . splitByCubes
        rowTiltEast = concat . map sort . splitByCubes
        tiltWest = map rowTiltWest
        tiltEast = map rowTiltEast
        tiltNorth = transpose . map rowTiltWest . transpose
        tiltSouth = transpose . map rowTiltEast . transpose

solve :: [[Char]] -> [[Char]]
solve rocks = iterate spin rocks !! 500

main :: IO ()
main = readFile "./input" >>= print . solve . lines