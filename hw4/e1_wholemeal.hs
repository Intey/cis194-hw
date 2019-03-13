fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n = summarize evenN
  where evenN = if even n then n else (3 * n + 1)
        summarize = sum . takeWhile (>1) . iterate (\x -> x `div` 2)

