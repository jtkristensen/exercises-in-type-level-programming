{-# LANGUAGE PolyKinds
           , TypeOperators
           , GADTs
           , RankNTypes
  #-}

module Equality where

import Data.Kind (Type)

infix 3 :==:

data a :==: b :: Type where
  Reflexive :: a :==: a

infixr 1 %|

(%|) :: (a :==: b) -> ((a ~ b) => r) -> r
(%|) Reflexive proof = proof

-- Useful lemmas and abbreviations:

qed :: a :==: a
qed = Reflexive

reflexive :: a :==: a
reflexive = Reflexive

symmetric :: a :==: b -> b :==: a
symmetric Reflexive = Reflexive

congruent :: (a :==: b) -> (f a :==: f b)
congruent Reflexive = Reflexive

transitive :: (a :==: b) -> (b :==: c) -> (a :==: c)
transitive Reflexive Reflexive = Reflexive
