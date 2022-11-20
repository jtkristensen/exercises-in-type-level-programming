{-# LANGUAGE TypeOperators
           , DataKinds
           , GADTs
           , NoImplicitPrelude
  #-}

module Properties where

import Nat
import Reasoning

plus0IdR
  :: SNat n
  -> n :+ 'Zero :==: n

plus0IdR  O
  = qed
plus0IdR (S n)
  = eqCongruent (plus0IdR n)

