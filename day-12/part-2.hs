import Control.Arrow ((&&&))
import Debug.Trace (trace)
import Control.Monad.Fix (fix)
import Data.MemoTrie (memoFix)
import MyHelpers (splitOn)


parse :: String -> [(String, [Int])]
parse = map ((head &&& (parseList . last)) . words) . lines
    where
        parseList = map read . splitOn (==',')

countArrF :: ((String, [Int]) -> Int) -> (String, [Int]) -> Int
countArrF _ ([], []) = 1
countArrF _ ([], _) = 0
countArrF _ (springs, []) = fromEnum $ all (\c -> c=='.' || c=='?') springs
countArrF countArr (('.':springs), records) =  countArr (springs, records)
countArrF countArr (('?':springs), records) = countArr (('.':springs), records) + countArr (('#':springs), records)
countArrF countArr (('#':springs), (r:records))
    | not chunkValid = 0
    | chunkValid && null charAfterChunk = countArr (afterChunk, records)
    | chunkValid && charAfterChunk == ['#'] = 0
    | chunkValid && charAfterChunk == ['.'] = countArr (afterChunk, records)
    | chunkValid && charAfterChunk == ['?'] = countArr (('.' : drop 1 afterChunk), records)
    where
        chunk = take r ('#':springs)
        afterChunk = drop r ('#':springs)
        charAfterChunk = take 1 afterChunk
        chunkValid = length chunk == r && all (\c -> c=='#' || c=='?') chunk

solveCase :: (String, [Int]) -> Int
solveCase (springs, records) = memoFix countArrF (unfoldSprings, unfoldRecords)
    where
        unfoldSprings = springs ++ "?" ++ springs ++ "?" ++ springs ++ "?" ++ springs ++ "?" ++ springs
        unfoldRecords = concat $ replicate 5 $ records

solve :: [(String, [Int])] -> Int
solve = sum . map solveCase

main :: IO ()
main = readFile "./input" >>= print . solve . parse

