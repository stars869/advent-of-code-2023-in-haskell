import Data.List (group, sort, sortBy)
import Data.Function (on)
import Data.Map (Map, fromList, (!))
import Debug.Trace (trace)

parse :: String -> [(String, Int)]
parse = map (toPair . words) . lines
    where
        toPair [hand, bid] = (hand, read @Int bid)

compareHands :: String -> String -> Ordering
compareHands hand1 hand2 = if handsTypeOrder == EQ then cardsOrder else handsTypeOrder
    where
        cardOrdMap = fromList $ zip (reverse "AKQJT98765432") [0..] :: Map Char Int
        handTypeOrderMap = sum . map ((^ 10) . length) . group . sort . map (cardOrdMap !) :: String -> Int
        handsTypeOrder = on compare handTypeOrderMap hand1 hand2
        cardsOrder = on compare (map (cardOrdMap !)) hand1 hand2

solve :: [(String, Int)] -> Int
solve  = sum . map (\(r, (h, b)) -> r * b) . zip [1..] . sortHands 
    where
        sortHands = sortBy (on compareHands fst)

main :: IO ()
main = readFile "./input" >>= print . solve . parse