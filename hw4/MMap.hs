module MMap where
mmap :: (a -> b) -> [a] -> [b]
mmap f xs = foldr (\x acc -> (f x):acc) [] xs
