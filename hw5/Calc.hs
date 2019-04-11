{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where 
import ExprT
import Parser
import qualified StackVM as VM
import qualified Data.Map as M
import Control.Applicative

-- Exercise 1 =========================================
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add (Lit a) (Lit b)) = a + b
eval (Mul (Lit a) (Lit b)) = a * b
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)


-- Exercise 2 =========================================
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              Nothing -> Nothing
              Just v -> Just $ eval v

-- Exercise 3 =========================================
--
-- under hood, we always works with Integer, because parser 
-- works with integer
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4 ========================================= 
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where 
  lit = (0 <=)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax l) (MinMax r) = MinMax (max l r)
  mul (MinMax l) (MinMax r) = MinMax (min l r)

instance Expr Mod7 where
  lit = Mod7 . abs . (mod 7)
  add (Mod7 l) (Mod7 r) = Mod7 $ (mod 7) $ (l + r)
  mul (Mod7 l) (Mod7 r) = Mod7 $ (mod 7) $ (l * r) 


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- Exercise 5 =========================================
instance Expr VM.Program where
  lit v= [VM.PushI v]
  add l r = l ++ r ++ [VM.Add]
  mul l r = l ++ r ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul 
  
-- Exercise 6 =========================================
class HasVars a where
  var :: String -> a
 
data VarExprT = LitV Integer
              | AddV VarExprT VarExprT
              | MulV VarExprT VarExprT
              | Var String
              deriving (Show, Eq)


instance Expr VarExprT where
  lit = LitV
  add = AddV
  mul = MulV

instance HasVars VarExprT where
  var = Var

-- class Expr a where
--   lit :: Integer -> a
--   add :: a -> a -> a
--   mul :: a -> a -> a

type MapApplicable = (M.Map String Integer -> Maybe Integer)
-- each function, should returns value of this function 
-- type. In other words - we bind actions, and leave them 
-- without concrete map
instance HasVars MapApplicable where
  var s = M.lookup s

-- lit - just return value. Didn't use map
-- add - (add (lit 3) (lit 2)) -> funciton, that 
-- add - l r - is functions
instance Expr MapApplicable where
  lit v = (\m -> Just v)
  add l r = (\m -> liftA2 (+) (l m) $ (r m))
  mul l r = (\m -> liftA2 (*) (l m) $ (r m))


withVars :: [(String, Integer)] -> MapApplicable -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
