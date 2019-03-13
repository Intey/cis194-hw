{-# OPTIONS_GHC -Wall #-}

type Height = Integer
data Tree a = Leaf 
              | Node Height (Tree a) a (Tree a)
    deriving (Eq)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf 

-- when insert, we should increase height, only if it's create new node one of
-- child.
-- First assumption - use max of current childs. It's ok for 1 height nodes.
-- Next - we insert one more node in child (and it's height become 2), but in
-- max we use old height, and left with incorrect height
-- Correctly it's will be to insert in childs first, and then get max height of
-- them
insert :: (Ord a) => a -> Tree a -> Tree a
insert item Leaf = (Node 0 Leaf item Leaf)
insert item (Node _ Leaf v r) = (Node 1 (insert item Leaf) v r)
insert item (Node _ l v Leaf) = (Node 1 l v (insert item Leaf))
insert item (Node _ l c r) 
  | lh > rh = (Node ((max (height l) rh)+1) l c newR)
  | lh == rh = (Node ((max (height l) rh)+1) l c newR)
  | otherwise = (Node ((max lh (height r))+1) newL c r) 
    where newR = (insert item r)
          newL = (insert item l)
          rh = height newR
          lh = height newL

-- returns node height
height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h


-- show tree with identation as:
-- > foldTree [1..7]
-- (Node 2 value: 7
-- (Node 1 value: 6
--     (Node 0 value: 2 _ _)
--     (Node 0 value: 1 _ _))
-- (Node 1 value: 5
--     (Node 0 value: 4 _ _)
--     (Node 0 value: 3 _ _)))
instance (Show a) => Show (Tree a) where
  show node = showIndentTree "  " 0 node

showIndentTree :: (Show a) => String -> Int -> (Tree a) -> String
showIndentTree sym indent Leaf = step ++ "_"
    where step = pad indent sym
showIndentTree sym indent (Node n Leaf v Leaf) = 
  step ++ "(Node " ++ show n ++ " value: " ++ show v ++ " _ _)"
    where step = pad indent sym
showIndentTree sym indent (Node n l v r) = 
  step ++ "(Node " ++ show n ++ " value: " ++ show v ++ "\n" 
  ++ step ++ (showIndentTree sym (succ indent) l) ++ "\n" 
  ++ step ++ (showIndentTree sym (succ indent) r) ++ ")"
    where step = pad indent sym

-- create identation of size
pad :: Int -> String -> String
pad indent = concat . replicate indent

