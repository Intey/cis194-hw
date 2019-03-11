import qualified Data.Map as M
type Counter = M.Map Integer Integer

histogram :: [Integer] -> String
histogram xs = foldr folder "\n=========\n123456789\n" [10,9..0]
  where c = counter xs
        folder = (\l s -> (showLine l c) ++ "\n" ++ s )

counter :: [Integer] -> Counter
counter xs = foldr (\x acc -> M.alter inc x acc) M.empty xs

-- [3, 3, 5]
--    █
--    █ █
-- ==========
-- 0123456789

inc :: Maybe Integer -> Maybe Integer
inc (Just value) = Just (succ value)
inc Nothing = Just 1

-- we can iterae over counter. We foldr over [10,9..0] as i. Then, we iterate
-- over each [1...9] as v and lookup them in counter. if it's exists - render
-- █, else - <space>
showLine :: Integer -> Counter -> String
showLine l m = foldr (\k acc -> (render $ lookGT l k m):acc) "" [1..9]
-- renders value of couter
render :: (Maybe v) -> Char
render (Just v) = '█'
render Nothing = ' '

lookGT :: Integer -> Integer -> Counter -> (Maybe Integer)
lookGT v k m = case (M.lookup k m) of 
  (Just i) -> if i > v then (Just i) else Nothing
  Nothing -> Nothing

