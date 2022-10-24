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

data Proof proposition = Program proposition

-- Examples

intro :: Proof ((a :/\: b) :\/: c) -> Proof ((a :\/: c) :/\: (b :\/: c))
intro (Program (Left (a, b))) = Program (Left a, Left b)
intro (Program (Right c    )) = Program (Right c, Right c)

-- Exercises

exercise1 :: Proof ((a :=>: b) :/\: (b :=>: c)) -> Proof (a :=>: c)
exercise1 (Program (f, g)) = Program (g . f)

exercise2 :: Proof (a :\/: b) -> Proof (a :=>: c) -> Proof (b :=>: c)  -> Proof c
exercise2 (Program (Left  a)) (Program f) _ = Program $ f a
exercise2 (Program (Right b)) _ (Program g) = Program $ g b



-- lem :: a :\/: (Not a)
-- lem = Right undefined
