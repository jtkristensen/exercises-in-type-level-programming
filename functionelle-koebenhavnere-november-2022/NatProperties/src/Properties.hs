{-# LANGUAGE TypeOperators
           , DataKinds
           , GADTs
  #-}

module Properties where

import Nat
import Equality


plus0
  :: SNat n
  -> n :+ 'Zero :==: n

plus0 = undefined

plusS
  :: SNat m -> SNat n
  -> m :+ 'Suc n :==: 'Suc m :+ n

plusS = undefined

plusCommutative
  :: SNat m -> SNat n
  -> m :+ n :==: n :+ m

plusCommutative = undefined

plusInjective
  :: SNat m -> SNat n -> SNat k
  -> m :==: n -> m :+ k :==: n :+ k

plusInjective = undefined

plusAssociative
  :: SNat m -> SNat n -> SNat k
  -> m :+ (n :+ k) :==: (m :+ n) :+ k

plusAssociative  = undefined
