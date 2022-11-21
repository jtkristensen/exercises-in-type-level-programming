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

zero = undefined

suc
  :: SNat m -> SNat n
  -> m :+ 'Suc n :==: 'Suc m :+ n

suc = undefined

commutative
  :: SNat m -> SNat n
  -> m :+ n :==: n :+ m

commutative = undefined

injective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :+ k :==: n :+ k

injective = undefined

associative
  :: SNat m -> SNat n -> SNat k
  -> m :+ (n :+ k) :==: (m :+ n) :+ k

associative = undefined
