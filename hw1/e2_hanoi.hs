module Hanoi (
hanoi,
) where

-- move discs from first pen to second
-- odd - target, even - other
--
-- |     |
-- |     |
-- |     |
--
-- |21   |2    |     | 
-- |     |     |2    |21
-- |     |1    |1    | 
-- 
--                   recur
-- |321  |32   |3    |3    |     |1    |1    |  
-- |     |1    |1    |     |3    |3    |32   |321
-- |     |     |2    |21   |21   |2    |     | 
--
--             *1          *2    *3          **    *4
-- |4321 |432  |43   |43   |4    |41   |41   |4    |     |     |2    |21   |21   |2    |     |
-- |     |     |2    |21   |21   |2    |     |     |4    |41   |41   |4    |43   |43   |432  |4321
-- |     |1    |1    |     |3    |3    |32   |321  |321  |32   |3    |3    |     |1    |1    |
-- |1, 3 |1, 2 |3, 2 |1, 3 |2, 1 |2, 3 |1, 3 |1, 2 |3, 2 |3, 1 |2, 1 |3, 2 |1, 3 |1, 2 |3, 2 |
-- |1, 3 |1, 2 |3, 2 |1, 3 |2, 1 |2, 3 |1, 3 |1, 2 |3, 2 |3, 1 |2, 1 |3, 2 |1, 3 |1, 2 |3, 2
-- *1 - now we need recurse from p3 to create move p3,p2
-- *2 - return to p1 to create p1,p3. top is odd, so we swap action order
-- *3 - go to p2, and move to p3, for free p2, and create tower on p3
-- ** - todo
-- ====TowerOf4==== ‖ ‖ ====TowerOf3==== ‖ ‖ ===TowerOf3s==== ‖
-- 0  3<-1 | 3 -> 2 ‖ ‖                  ‖ ‖                  ‖
-- 1->2  1 | 1 -> 2 ‖ ‖                  ‖ ‖                  ‖
-- 2--2->0 | 1 -> 3 ‖ ‖                  ‖ ‖                  ‖
--                  ‖ ‖                  ‖ ‖                  ‖
-- 2  1<-1 | 3 -> 2 ‖ ‖                  ‖ ‖                  ‖
-- 1<-2  1 | 2 -> 1 ‖ ‖                  ‖ ‖                  ‖
-- 0<-2--2 | 3 -> 1 ‖ ‖                  ‖ ‖                  ‖
--                  ‖ ‖                  ‖ ‖                  ‖
-- 0  1<-3 | 3 -> 2 ‖ ‖                  ‖ ‖                  ‖
-- 1->0  3 | 1 -> 2 ‖ ‖                  ‖ ‖                  ‖
-- 2--0->2 | 1 -> 3 ‖ ‖ 1->2  0 | 1 -> 2 ‖ ‖ 1--0->2 | 1 -> 3 ‖
--                  ‖ ‖                  ‖ ‖                  ‖
-- 2  1->1 | 2 -> 3 ‖ ‖ 1  1<-1 | 3 -> 2 ‖ ‖ 1  1->1 | 2 -> 3 ‖     
-- 1<-2  1 | 2 -> 1 ‖ ‖ 0<-1--2 | 3 -> 1 ‖ ‖ 0<-2  1 | 2 -> 1 ‖     
-- 2--2->0 | 1 -> 3 ‖ ‖ 1->0  2 | 1 -> 2 ‖ ‖ 1--2->0 | 1 -> 3 ‖
--                  ‖ ‖                  ‖ ‖                  ‖ 
-- 2  1<-1 | 3 -> 2 ‖ ‖ 1  1->1 | 2 -> 3 ‖ ‖ 1  1<-1 | 3 -> 2 ‖ 
-- 3->0  1 | 1 -> 2 ‖ ‖ 2--1->0 | 1 -> 3 ‖ ‖ 2->0  1 | 1 -> 2 ‖ 
-- 4--0->0 | 1 -> 3 ‖ ‖ 3->0  0 | 1 -> 2 ‖ ‖ 3--0->0 | 1 -> 3 ‖  
--
-- TowerOf3s and TowerOf3 is same, but "3s" is has swapped swap 2<->3 peg. Now
-- its actions is same for TowerOf4. Taking this, we can pretend that TowerOf5
-- can be done with TowerOf4s and next steps...
--
--                  ‖     1                             
-- 0  4  0 | 3 -> 2 ‖    -2-                       
-- 0  4<-1 | 3 -> 2 ‖   --3--   ----5----  ---4---  
-- 2<-1--2 | 3 -> 2 ‖ ────┴──── ────┴──── ────┴────                         
-- 1<-2  2 | 2 -> 1 ‖                                                         
-- 1  3->1 | 2 -> 3 ‖               1    
--                  ‖              -2-              
--                  ‖   --3--   ----5----  ---4---   
-- 0<-3--2 | 3 -> 1 ‖ ────┴──── ────┴──── ────┴──── 
--                  ‖
--                  ‖                         1
-- 1->2  2 | 1 -> 2 ‖                        -2-
-- 1  1<-3 | 3 -> 2 ‖                       --3--
-- 0<-1--4 | 3 -> 1 ‖     │     ----5----  ---4---
-- 1->0  4 | 1 -> 2 ‖ ────┴──── ────┴──── ────┴────
-- ====TowerOf4s=== ‖
--
-- maybe split task in tasks of creation lower towers? first 3 steps create
-- tower "21" on p2 
-- next we create tower 321 from tower 21, and 3. So we just move p1,p3, next
-- we move to hanoi 2 p2 p3 p1
-- we can do this, because tower "21" was created earlier, and it's can't has
-- wider disc than on other pegs.
type Peg = String
type Move = (Peg, Peg)

type DiscsCount = Integer
type TargetPeg = Peg
type SourcePeg = Peg
type OtherPeg = Peg

-- First steps should move all discs to other peg exept biggest disc. Second
-- step - move biggest. Last step - move rest discs on other peg to destination 
hanoi :: DiscsCount -> SourcePeg -> TargetPeg -> OtherPeg -> [Move]
hanoi d p1 p2 p3
  | d == 0 = []
  | otherwise = (hanoi (d-1) p1 p3 p2) ++ [(p1, p2):(hanoi (d-1) p3 p2 p1)
