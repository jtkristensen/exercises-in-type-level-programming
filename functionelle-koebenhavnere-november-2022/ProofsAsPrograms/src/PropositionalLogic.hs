{-# LANGUAGE TypeOperators #-}

module PropositionalLogic where

-- Essential.
data Bottom                 -- A type with no constructors.
type Top = ()               -- The unit type.
type a :/\: b = (a, b)      -- Conjunction is a pair.
type a :\/: b = Either a b  -- Disjunction is a sum.
type a :=>: b = a -> b      -- Implication is a function.

-- Derived connectives.
type Not a      = a :=>: Bottom                -- Negation.
type a :<=>: b  = ((a :=>: b) :/\: (b :=>: a)) -- Biimplication.

data Proposition program = Proof program
type Premise    = Proposition
type Conclusion = Proposition

-- Examples

example1
  ::      Premise a    ->    Premise b
       ----------------------------------
  ->      Conclusion (a :/\: b)

example1 (Proof a) (Proof b) = Proof (a, b)


example2
  ::   Premise (a :=>: b) -> Premise a
       ---------------------------------------
  ->            Conclusion  b

example2 (Proof f) (Proof a) = Proof (f a)

-- Exercises


exercise1
  ::   Premise ((a :=>: b) :/\: (b :=>: c))
     -------------------------------------------
  ->      Conclusion  (a :=>: c)

exercise1 = undefined

exercise2
  :: Premise ((a :\/: b) :/\: (a :=>: c) :/\: (b :=>: c))
     --------------------------------------------------------------
  ->                      Conclusion c

exercise2 = undefined

exercise3
  ::            Premise ((a :/\: b) :\/: c)
     -------------------------------------------------
  ->      Conclusion ((a :\/: c) :/\: (b :\/: c))

exercise3 = undefined

