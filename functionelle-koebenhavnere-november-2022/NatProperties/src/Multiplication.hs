{-# LANGUAGE TypeOperators
           , DataKinds
           , GADTs
  #-}

module Multiplication where

import Nat
import Equality
import qualified Addition

zero
  :: SNat n
  -> n :* 'Zero :==: 'Zero

zero  O    = Reflexive
zero (S n) = zero n

-- no suc rule

commutative
  :: SNat m -> SNat n
  -> m :* n :==: n :* m

commutative  O     n    = symmetric (zero n)
commutative (S m)  O    = zero m
commutative (S m) (S n) =
     commutative m (S n)
  %| commutative n (S m)
  %| commutative n m
  %| Addition.associative n m (m %* n)
  %| Addition.associative m n (m %* n)
  %| Addition.commutative n m
  %| qed

injective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :* k :==: n :* k

injective _ _ _ Reflexive = qed

distributive
  :: SNat m -> SNat n -> SNat k
  -> (m :+ n) :* k :==: (m :* k) :+ (n :* k)

distributive O _ _ = qed
distributive (S m) n k =
     distributive m n k
  %| Addition.associative k (m %* k) (n %* k)
  %| qed

associative
  :: SNat m -> SNat n -> SNat k
  -> m :* (n :* k) :==: (m :* n) :* k

associative = undefined

