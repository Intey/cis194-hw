module Xor where

-- RS-Trigger, boolean type - count to 2. One True - 1, 2 True - 10, so 0. 
-- And so on
xor :: [Bool] -> Bool
xor = foldr (\x acc -> acc /= x) False 
