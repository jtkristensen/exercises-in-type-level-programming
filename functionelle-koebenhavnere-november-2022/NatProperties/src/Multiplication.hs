{-# LANGUAGE TypeOperators
           , DataKinds
           , GADTs
  #-}

module Multiplication where

import Nat
import Equality
-- import qualified Addition

zero
  :: SNat n
  -> n :* 'Zero :==: 'Zero

zero = undefined

-- no suc rule

commutative
  :: SNat m -> SNat n
  -> m :* n :==: n :* m

commutative = undefined

injective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :* k :==: n :* k

injective _ _ _ Reflexive = qed

distributive
  :: SNat k -> SNat m -> SNat n
  -> k :* (m :+ n) :==: (k :* n) :+ (k :* m)

distributive = undefined

associative
  :: SNat m -> SNat n -> SNat k
  -> m :* (n :* k) :==: (m :* n) :* k

associative  = undefined
