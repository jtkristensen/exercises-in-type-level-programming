{-# LANGUAGE PolyKinds
           , TypeFamilies
           , TypeOperators
           , GADTs
           , RankNTypes
  #-}

module Reasoning where

import Data.Kind (Type)

data family Singleton ( t :: k )

infix 3 :==:

data a :==: b :: Type where
  Reflexive :: a :==: a

infixr 1 %|

(%|) :: (a :==: b) -> ((a ~ b) => r) -> r
(%|) Reflexive x = x

eqSymmetric :: a :==: b -> b :==: a
eqSymmetric Reflexive = Reflexive

eqCongruent :: (Singleton a -> Singleton b) -> (a :==: b) -> (f a :==: f b)
eqCongruent _ Reflexive = Reflexive

eqTransitive :: (a :==: b) -> (b :==: c) -> (a :==: c)
eqTransitive Reflexive Reflexive = Reflexive

qed :: a :==: a
qed = Reflexive
