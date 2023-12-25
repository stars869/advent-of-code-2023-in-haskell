import Data.List (group, sort, sortBy)
import Data.Function (on)
import Data.Map (Map, fromList, (!))
import Debug.Trace (trace)

parse :: String -> [(String, Int)]
parse = map (toPair . words) . lines
    where
        toPair [hand, bid] = (hand, read @Int bid)

compareHands :: String -> String -> Ordering
compareHands hand1 hand2 = mappend handsTypeOrder cardsOrder
    where
        cardOrdMap = fromList $ zip (reverse "AKQT98765432J") [0..]
        countJ = length . filter (=='J')
        groupCards = (\l -> l++[0]) . map length . group . sort . map (cardOrdMap !) . filter (/='J')
        handTypeOrderMap hand = sum $ map (^10) $ (\(x:xs) -> (x + countJ hand):xs) $ reverse $ sort $ groupCards hand 
        handsTypeOrder = on compare handTypeOrderMap hand1 hand2
        cardsOrder = on compare (map (cardOrdMap !)) hand1 hand2

solve :: [(String, Int)] -> Int
solve  = sum . zipWith (*) [1..] . map snd . sortHands 
    where
        sortHands = sortBy (on compareHands fst)

main :: IO ()
main = readFile "./input" >>= print . solve . parse