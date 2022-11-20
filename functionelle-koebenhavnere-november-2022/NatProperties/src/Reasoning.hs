{-# LANGUAGE PolyKinds
           , TypeOperators
           , GADTs
           , RankNTypes
  #-}

module Reasoning where

import Singleton
import Data.Kind (Type)

infix 3 :==:

data a :==: b :: Type where
  Reflexive :: a :==: a

infixr 1 %|

(%|) :: (a :==: b) -> ((a ~ b) => r) -> r
(%|) Reflexive x = x

qed :: a :==: a
qed = Reflexive

-- Useful lemmas:

eqSymmetric :: a :==: b -> b :==: a
eqSymmetric Reflexive = Reflexive

eqCongruent :: (a :==: b) -> (f a :==: f b)
eqCongruent Reflexive = Reflexive

eqTransitive :: (a :==: b) -> (b :==: c) -> (a :==: c)
eqTransitive Reflexive Reflexive = Reflexive
