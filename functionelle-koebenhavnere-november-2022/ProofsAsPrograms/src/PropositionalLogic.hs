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
  :: Premise ((a :=>: b) :/\: (b :=>: c))
  -------------------------------------------
  -> Conclusion  (a :=>: c)
exercise1 (Proof (f, g)) = Proof (g . f)

exercise2
  :: Premise    (a :\/: b)
  -> Premise    (a :=>: c)
  -> Premise    (b :=>: c)
  -> Conclusion c
exercise2 (Proof (Left  a)) (Proof f) _ = Proof $ f a
exercise2 (Proof (Right b)) _ (Proof g) = Proof $ g b

exercise3
  :: Premise    ((a :/\: b) :\/: c)
  -> Conclusion ((a :\/: c) :/\: (b :\/: c))
exercise3 (Proof (Left (a, b))) = Proof (Left a, Left b)
exercise3 (Proof (Right c    )) = Proof (Right c, Right c)


