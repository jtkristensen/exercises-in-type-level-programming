{-# LANGUAGE TypeOperators
           , DataKinds
           , GADTs
           , NoImplicitPrelude
  #-}

module Properties where

import Nat
import Equality

plus0
  :: SNat n
  -> n :+ 'Zero :==: n

plus0  O    = reflexive
plus0 (S n) = congruent (plus0 n)

plusS
  :: SNat m -> SNat n
  -> m :+ 'Suc n :==: 'Suc m :+ n

plusS  O    _ = reflexive
plusS (S m) n =
  congruent (plusS m n)

plusCommutative
  :: SNat m -> SNat n
  -> m :+ n :==: n :+ m

plusCommutative  O    n = symmetric (plus0 n)
plusCommutative (S m) n =
     plusCommutative n m
  %| symmetric (plusS n m)

plusInjective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :+ k :==: n :+ k

plusInjective _ _ _ proof = proof %| qed

plusAssociative
  :: SNat m -> SNat n -> SNat k
  -> m :+ (n :+ k) :==: (m :+ n) :+ k

plusAssociative  O    _ _ = qed
plusAssociative (S m) n k =
  congruent (plusAssociative m n k)
