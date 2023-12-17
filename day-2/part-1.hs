import System.IO
import Distribution.Utils.String (trim)
import Data.Map (Map, fromList, lookup)
import Data.Maybe

import MyHelpers ( wordsWhen )

import Prelude hiding (lookup)

data Cubes = Cubes
    {
        red :: Int,
        green :: Int,
        blue :: Int
    }

maxCubes :: Cubes
maxCubes = Cubes 12 13 14

cubesLE :: Cubes -> Cubes -> Bool
cubesLE cubes1 cubes2 = (red cubes1 <= red cubes2)
    && (green cubes1 <= green cubes2)
    && (blue cubes1 <= blue cubes2)

cubesAdd :: Cubes -> Cubes -> Cubes
cubesAdd (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (r1 + r2) (g1 + g2) (b1 + b2)

readColor :: String -> Cubes
readColor colorStr = case color of
    "red" -> Cubes num 0 0
    "green" -> Cubes 0 num 0
    "blue" -> Cubes 0 0 num
    where
        subStrs = words (trim colorStr)
        num = read (head subStrs)
        color = last subStrs

readCubes :: String -> Cubes
readCubes cubesStr = foldl cubesAdd (Cubes 0 0 0) $ map readColor $ wordsWhen (== ',') cubesStr

readRounds :: String -> [Cubes]
readRounds roundsStr = map readCubes $ wordsWhen (== ';') roundsStr

solveCase :: String -> Int
solveCase caseStr = if possible then gameID else 0
    where
        subStrs = wordsWhen (== ':') caseStr
        gameID = read $ last $ words $ head subStrs
        possible = all (`cubesLE` maxCubes) (readRounds $ last subStrs)


main :: IO ()
main = do
    input <-  readFile "./input"
    let result = sum $ map solveCase $ lines input
    print result
