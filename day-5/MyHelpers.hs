module MyHelpers
(
    splitOn,
    splitEveryTwo
) where

splitOn     :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'


splitEveryTwo :: [a] -> [(a, a)]
splitEveryTwo [] = []
splitEveryTwo (x:y:rest) = (x, y) : splitEveryTwo rest
splitEveryTwo _ = error "Incomplete pair"
