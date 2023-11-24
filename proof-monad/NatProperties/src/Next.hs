{-# LANGUAGE GADTs
           , DataKinds
           , PolyKinds
           , TypeOperators
           , EmptyCase
#-}

module Next where

import Nat hiding ((+))
import Data.Kind (Type)
import Comparison

-- Intrinsic typing.
data Expr a where
  Const :: b -> Expr b
  Less  :: Ord c => Expr c -> Expr c -> Expr Bool
  Add   :: Num n => Expr n -> Expr n -> Expr n
  If    :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (Const b) = b
eval (Less e1 e2) = eval e1 < eval e2
eval (Add e1 e2) = eval e1 + eval e2
eval (If e1 e2 e3) =
  eval (if eval e1 then e2 else e3)

-- Size types.
data Vector :: Nat -> (Type -> Type) where
  Empty :: Vector 'Zero a
  Cons  :: a -> Vector n a -> Vector ('Suc n) a

h :: Vector ('Suc n) a -> a
h (Cons a _) = a

t :: Vector ('Suc n) a -> Vector n a
t (Cons _ rest) = rest

append :: Vector m a -> Vector n a -> Vector (m :+ n) a
append Empty ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

find :: Vector m a -> n `Le` m -> a
find Empty proof = case proof of {}
find (Cons x _) (ZLeS _) = x
find (Cons _ xs) (SLeS proof) = find xs proof
