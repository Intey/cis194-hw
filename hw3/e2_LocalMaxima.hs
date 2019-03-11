module LocalMaxima(
localMaxima
) where 

-- finds local maximums - element that greater it's siblings.
-- if element has no sibling(first, and last elements of list) - they is can't 
-- be maximum
-- Realization notes: 
-- use foldr, because we prepend each element, but we need to save order.
localMaxima :: [Integer] -> [Integer]
localMaxima source = foldr 
  (\x acc -> if isMax 3 x then (x !! (mod (length x) 2)):acc else acc)
  [] (makeChunks 3 source)

-- Creates chunks of array
makeChunks :: Int -> [a] -> [[a]]
makeChunks count xs = mch True count xs

-- makeChunksPrivate - is realizaion of makeChunks, but needed for detect first
-- step and return "screen" with focus on first element. If count is 3, and xs
-- is [,1,2,3,4], so first chunk should be [1,2], because screen see left,
-- unexist part [_, 1, 2], 3, 4)
mch :: Bool -> Int -> [a] -> [[a]]
mch isFirst count xs
  | length xs == 0 = [] -- prevent infinitive loop
  | count > (length xs) = [xs] -- prevent get last as separate element
  | isFirst = (take (count-1) xs):(mch False count xs)
  | not isFirst = (take count xs):(mch False count $ drop 1 xs)

-- take first and it's left and right
-- [_, 1, 2], 3, 4, 5, _
-- _, [1, 2, 3], 4, 5, _
-- _, 1, [2, 3, 4], 5, _
-- _, 1, 2, [3, 4, 5], _
-- _, 1, 2, 3, [4, 5, _]

isMax :: (Ord a) => Int -> [a] -> Bool
isMax _ [] = False
isMax count xs 
  | count /= (length xs) = False -- if no left or right elements of center
  | otherwise = if centerIdx < (length xs) 
  then (xs !! centerIdx) == (maximum xs)
  else False
  where 
    centerIdx = mod (length xs) 2

