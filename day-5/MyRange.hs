module MyRange
(
    Range, 
    shift,
    intersect,
    substract,
    substractDomain
) where


type Range = (Int, Int)

shift :: Int -> Range -> Range
shift s (start, end) = (start + s, end + s)

contains :: Range -> Int -> Bool 
contains r i = fst r <= i && i <= snd r 

intersect :: Range -> Range -> Maybe Range 
intersect r1 r2 
    | snd r1 < fst r2 = Nothing
    | fst r1 > snd r2 = Nothing
    | contains r1 (fst r2) && contains r1 (snd r2) = Just r2 
    | contains r2 (fst r1) && contains r2 (snd r1) = Just r1  
    | contains r1 (fst r2) && not (contains r1 (snd r2)) = Just (fst r2, snd r1)
    | contains r1 (snd r2) && not (contains r1 (fst r2)) = Just (fst r1, snd r2)
    | otherwise = error "invalid range" 

substract :: Range -> Range -> [Range]
substract r1 r2 
    | fst r1 > snd r2 = [r1]
    | snd r1 < fst r2 = [r1]
    | fst r1 < fst r2 && snd r1 >= fst r2 && snd r1 <= snd r2 = [(fst r1, fst r2 - 1)]
    | fst r1 < fst r2 && snd r1 > snd r2 = [(fst r1, fst r2 - 1), (snd r2 + 1, snd r1)]
    | fst r1 >= fst r2 && fst r1 <= snd r2 && snd r1 > snd r2 = [(snd r2 + 1, snd r1)]
    | otherwise = [] 


type Domain = [Range]

substractDomainByRange :: Domain -> Range -> Domain 
substractDomainByRange domain range = concatMap (\r -> substract r range) domain

substractDomain :: Domain -> Domain -> Domain
substractDomain d1 d2 = foldl substractDomainByRange d1 d2
