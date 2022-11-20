{-# LANGUAGE TypeOperators
           , DataKinds
           , TypeFamilies
           , GADTs
           , NoImplicitPrelude
           , UndecidableInstances
  #-}


module Nat where

import Singleton
import Data.Kind (Type)

data Nat = Zero | Suc Nat

type SNat (n :: Nat) = Singleton n

data instance Singleton :: Nat -> Type where
  O :: Singleton 'Zero
  S :: Singleton n -> Singleton ('Suc n)

-- Addition:

infix 5 +, :+, %+

(+) :: Nat -> Nat -> Nat
Zero    + n = n
(Suc m) + n = Suc (m + n)

type family (n :: Nat) :+ (m :: Nat) :: Nat where
  'Zero    :+ n = n
  ('Suc m) :+ n = 'Suc (m :+ n)

(%+) :: SNat m -> SNat n -> SNat (m :+ n)
O     %+ n = n
(S m) %+ n = S (m %+ n)

-- Multiplication:

infix 6 *, :*, %*

(*) :: Nat -> Nat -> Nat
Zero    * _ = Zero
(Suc m) * n = n + (m * n)

type family (n :: Nat) :* (m :: Nat) :: Nat where
  'Zero    :* n = 'Zero
  ('Suc m) :* n = n :+ (m :* n)

(%*) :: SNat m -> SNat n -> SNat (m :* n)
O     %* _ = O
(S m) %* n = n %+ (m %* n)
