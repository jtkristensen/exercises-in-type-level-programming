{-# LANGUAGE TypeOperators
           , DataKinds
           , GADTs
  #-}

module Addition where

import Nat
import Equality

zero
  :: SNat n
  -> n :+ 'Zero :==: n

zero  O     = Reflexive
zero (S n1) = congruent $ zero n1

suc
  :: SNat m -> SNat n
  -> m :+ 'Suc n :==: 'Suc m :+ n

suc  O     _ = qed
suc (S n1) n =
  congruent (suc n1 n)

injective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :+ k :==: n :+ k

injective _ _ _ Reflexive = qed

associative
  :: SNat m -> SNat n -> SNat k
  -> m :+ (n :+ k) :==: (m :+ n) :+ k

associative  O    _ _ = qed
associative (S m) n k =
    congruent (associative m n k)

commutative
  :: SNat m -> SNat n
  -> m :+ n :==: n :+ m

commutative O     n = symmetric (zero n)
commutative (S m) n = commutative n m %| symmetric (suc n m)

