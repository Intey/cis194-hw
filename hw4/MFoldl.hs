module MFoldl where

-- my foldl from foldr
-- Can't find other solution with using foldr
mfoldl :: (a -> b -> a) -> a -> [b] -> a
mfoldl f init = foldr (flip f) init . reverse

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- traverse from right to left
-- foldr f 0 [1,2,3,4,5] == (f 1 (f 2 (3 f (f 4 (f 5 0)))))
-- with infix
-- foldr f 0 [1,2,3,4,5] == (1 `f` (2 `f` (3 `f` (4 `f` (5 `f` 0)))))
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- traverse from left to right
-- foldl f 0 [1,2,3,4,5] == (f (f (f (f (f 0 1) 2) 3) 4) 5)
-- with infix
-- foldl f 0 [1,2,3,4,5] == (((((0 `f` 1) `f` 2) `f` 3) `f` 4) `f` 5)

