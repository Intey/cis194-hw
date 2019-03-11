module Golf where

-- Convert source list to lists of Nth elements: 
-- every first, every second, every third, etc.
-- example: 
-- 
--     > skips "1234567890" 
--     ["1234567890", "24680", "369", "48", "50", "6", "7", "8", "9", "0"]
--
skips :: [a] -> [[a]]
skips [] = []
skips src = map (takeNth src) [1..size] 
  where size = length src

-- takes every n element of list
takeNth :: (Integral b) => [a] -> b -> [a]
takeNth [] _ = []
-- takeNth _ 0 = [] -- can be thown away, as takeNth is private
takeNth xs 1 = xs
takeNth xs n = foldr (selectNth n) [] (zip xs [1..])

-- put in acc element if n is element index divider otherwise just returns acc
selectNth :: (Integral b) => b -> (a, b) -> [a] -> [a]
selectNth n (v, i) acc
  | i `mod` n == 0 = v:acc 
  | otherwise = acc

