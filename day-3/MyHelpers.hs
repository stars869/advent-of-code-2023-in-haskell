module MyHelpers
(
    splitOn,
    conv2D,
    padding2D
) where

splitOn     :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'

takeRows :: Int -> [[a]] -> [[a]]
takeRows = take

takeColumns :: Int -> [[a]] -> [[a]]
takeColumns n = map (take n)

conv2DOnRow :: Int -> ([[a]] -> b) -> [[a]] -> [b]
conv2DOnRow kSize kernelFunc mat
    | length (head mat) < kSize = []
    | otherwise = val1 : conv2DOnRow kSize kernelFunc (map tail mat)
    where val1 = kernelFunc (takeColumns kSize (takeRows kSize mat))

conv2D :: Int -> ([[a]] -> b) -> [[a]] -> [[b]]
conv2D kSize kernelFunc mat 
    | length mat < kSize = []
    | otherwise = row1 : conv2D kSize kernelFunc (tail mat)
    where row1 = conv2DOnRow kSize kernelFunc mat

padding1D :: Int -> a -> [a] -> [a]
padding1D size value list = (replicate size value) ++ list ++ (replicate size value)

padding2D :: Int -> a -> [[a]] -> [[a]]
padding2D size value mat = replicate size (replicate (nColumns + size * 2) value) ++ map (\r -> padding1D size value r) mat ++ replicate size (replicate (nColumns + size * 2) value)
    where nColumns = length (head mat)
