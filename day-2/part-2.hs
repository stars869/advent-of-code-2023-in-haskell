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
solveCase (_, reveals) = power
    where
        getCubeMax color = maximum . map snd . filter ((==color) . fst) . concat
        power = product $ map (`getCubeMax` reveals) ["red", "green", "blue"]

solve :: [(Int, [[(String, Int)]])] -> Int
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse