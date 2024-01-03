module MyHelpers
(
    splitOn,
    takeEveryTwo
) where

splitOn     :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'


takeEveryTwo :: [a] -> [(a, a)]
takeEveryTwo [] = []
takeEveryTwo [_] = []
takeEveryTwo (x1:x2:xs) = (x1, x2) : takeEveryTwo xs