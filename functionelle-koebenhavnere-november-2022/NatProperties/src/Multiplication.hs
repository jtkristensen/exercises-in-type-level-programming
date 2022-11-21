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

zero  O    = qed
zero (S n) = zero n

-- no suc rule

commutative
  :: SNat m -> SNat n
  -> m :* n :==: n :* m

commutative  O    n = symmetric (zero n)
commutative (S m) O = commutative m O
commutative (S m) (S n) =
  congruent $
     commutative m (S n)
  %| commutative n (S m)
  %| commutative n    m
  %| Addition.associative m n (m %* n)
  %| Addition.associative n m (m %* n)
  %| Addition.commutative m n
  %| qed

injective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :* k :==: n :* k

injective = undefined

associative
  :: SNat m -> SNat n -> SNat k
  -> m :* (n :* k) :==: (m :* n) :* k

associative = undefined
