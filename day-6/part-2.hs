import Control.Arrow ((&&&))
import MyHelpers (splitOn)


parse :: String -> (Int, Int)
parse = (parseLine . head &&& parseLine . last) . lines
    where parseLine = read . filter (/=' ') . last . splitOn (==':')

solve :: (Int, Int) -> Int
solve (t, d) = floor r1 - ceiling r2 + 1
    where
        (t', d') = (fromIntegral t, fromIntegral d)
        sqrtDisc = sqrt (t' ^ 2 - (4 * d'))
        (r1, r2) = ((t' + sqrtDisc) / 2, (t' - sqrtDisc) / 2)

main :: IO ()
main = do
    readFile "./input" >>= print . solve . parse