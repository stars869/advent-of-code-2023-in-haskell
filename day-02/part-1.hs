import Distribution.Utils.String (trim)
import Control.Arrow ((&&&))
import MyHelpers (wordsWhen)


parse :: String -> [(Int, [[(String, Int)]])]
parse = map parseLine . lines
    where
        parseLine = (parseGameID &&& parseReveals) . wordsWhen (==':')
        parseGameID = read @Int . last . words . head
        parseReveals = map parseReveal . wordsWhen (==';') . last
        parseReveal = map parseCube . wordsWhen (==',')
        parseCube = (last &&& (read @Int . head)) . words . trim

solveCase :: (Int, [[(String, Int)]]) -> Int
solveCase (gameID, reveals) = gameID * fromEnum isPossible
    where
        getCubeMax color = maximum $ map snd $ filter ((==color) . fst) $ concat reveals
        isPossible = all (\(color, maxVal) -> getCubeMax color <= maxVal) [("red", 12), ("green", 13), ("blue", 14)]

solve :: [(Int, [[(String, Int)]])] -> Int
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse