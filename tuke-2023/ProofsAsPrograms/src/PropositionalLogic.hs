{-# LANGUAGE TypeOperators, EmptyCase #-}

module PropositionalLogic where

-- Essential.
data Bottom                 -- A type with no constructors.
type Top      = ()          -- The unit type.
type a :/\: b = (a, b)      -- Conjunction is a pair.
type a :\/: b = Either a b  -- Disjunction is a sum.
type a :=>: b = a -> b      -- Implication is a function.

-- Derived connectives.
type Not a      = a :=>: Bottom                -- Negation.
type a :<=>: b  = ((a :=>: b) :/\: (b :=>: a)) -- Biimplication.

-- Propositions as types. Proofs as programs.
data Proposition program = Proof program
type Premise    = Proposition
type Conclusion = Proposition

-- Convinient helper function.
derive :: Proposition something -> something
derive (Proof thing) = thing

-- Examples

example1
  ::      Premise a    ->    Premise b
       ----------------------------------
  ->      Conclusion (a :/\: b)

example1 (Proof a) (Proof b) = undefined

example2
  :: Premise         ((a :=>: b), a)
                  -----------------------
  -> Conclusion             b

example2 (Proof (f, a)) = Proof $ f a

example3
  :: Premise          Bottom
                ---------------------
  -> Conclusion      anything

example3 (Proof bottom) = undefined

-- Exercises

exercise1
  :: Premise        ((a :=>: b) , (b :=>: c))
                  ---------------------------------
  -> Conclusion              (a :=>: c)

exercise1 (Proof (f, g)) = undefined

exercise2
  :: Premise       ((a :\/: b) , (a :=>: c) , (b :=>: c))
                 ------------------------------------------------
  -> Conclusion                         c

exercise2 (Proof (aOrB, f, g)) = undefined

exercise3
  :: Premise          ((a :/\: b) :\/: c)
                  ------------------------------
  -> Conclusion    ((a :\/: c) :/\: (b :\/: c))

exercise3 (Proof outer) = undefined

exercise4
  :: Premise          ((a :\/: b) :/\: c)
                  ------------------------------
  -> Conclusion    ((a :/\: c) :\/: (b :/\: c))

exercise4 (Proof (aOrB, c)) = undefined

exercise5
  :: Premise        (p , Not p)
                 --------------------
  -> Conclusion       Bottom

exercise5 (Proof (p, np)) = undefined

exercise6
  :: Premise       ( p :\/: (Not p) , p :=>: q, Not p :=>: q)
                 ---------------------------------------------
  -> Conclusion                        q

exercise6 (Proof (pOrNP, f, g)) = undefined

-- Harder exercise

class Decidable p where
  lem :: p :\/: (Not p)

exercise7
  :: Decidable p
  => Conclusion  (( p :=>: q ) :<=>: ((Not p) :\/: q))

exercise7 = undefined

