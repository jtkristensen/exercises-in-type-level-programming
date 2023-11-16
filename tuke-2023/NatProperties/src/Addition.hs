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

zero  O     = qed
zero (S n1) = zero n1 %| qed

suc
  :: SNat m -> SNat n
  -> m :+ 'Suc n :==: 'Suc m :+ n

suc  O     _ = qed
suc (S n1) n = suc n1 n %| qed

injective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :+ k :==: n :+ k

injective _ _ _ Reflexive = qed

associative
  :: SNat m -> SNat n -> SNat k
  -> m :+ (n :+ k) :==: (m :+ n) :+ k

associative  O     _ _ = qed
associative (S n1) n k = associative n1 n k %| qed

commutative
  :: SNat m -> SNat n
  -> m :+ n :==: n :+ m

commutative  O     n = symmetric (zero n)
commutative (S n1) n =
     commutative n n1
  %| suc n n1
  %| qed
