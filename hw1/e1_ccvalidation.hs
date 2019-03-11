-- converts number to array of it's digits
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
  | n > 0 = (toDigits (div n 10)) ++ [(n `mod` 10)]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n > 0 = (n `mod` 10):(toDigitsRev (n `div` 10))
  | otherwise = []


-- performance: O(n*3), memory: O(1)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse . doubleImpl . reverse $ xs

doubleImpl :: [Integer] -> [Integer]
doubleImpl [] = []
doubleImpl (x:y:xs) = x:y*2:(doubleImpl xs)
doubleImpl (x:[]) = [x]


-- CPU optimized implemetations with using more memory
-- performance: O(n), memory: N*sizeof(bool)
-- on each step init on stack bool [MEM: Accumulator(prev+new)], and return
-- prev on next step 
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' xs = fst $ foldr oneTwoFold ([], False) xs

type Accumulator = ([Integer], Bool)
oneTwoFold :: Integer -> Accumulator -> Accumulator
oneTwoFold x (acc, bool) = (newX : acc, not bool) where
  newX = (if bool then x*2 else x)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9 = sumDigits (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate cn = hash `mod` 10 == 0 where
  hash = hasher cn
  hasher = sumDigits . doubleEveryOther . toDigits 
