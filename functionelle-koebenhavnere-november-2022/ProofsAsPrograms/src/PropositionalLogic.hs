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

-- Examples

example1
  ::      Premise a    ->    Premise b
       ----------------------------------
  ->      Conclusion (a :/\: b)

example1 (Proof a) (Proof b) = Proof (a, b)

example2
  :: Premise         ((a :=>: b), a)
                  -----------------------
  -> Conclusion             b

example2 (Proof (f, a)) = Proof $ f a

example3
  :: Premise          Bottom
                ---------------------
  -> Conclusion      anything

example3 (Proof bottom) = case bottom of {}

-- Exercises

exercise1
  :: Premise        ((a :=>: b) , (b :=>: c))
                  ---------------------------------
  -> Conclusion              (a :=>: c)

exercise1 = undefined

exercise2
  :: Premise       ((a :\/: b) , (a :=>: c) , (b :=>: c))
                 ------------------------------------------------
  -> Conclusion                         c

exercise2 = undefined

exercise3
  :: Premise          ((a :/\: b) :\/: c)
                  ------------------------------
  -> Conclusion    ((a :\/: c) :/\: (b :\/: c))

exercise3 = undefined

exercise4
  :: Premise          ((a :\/: b) :/\: c)
                  ------------------------------
  -> Conclusion    ((a :/\: c) :\/: (b :/\: c))

exercise4 = undefined


exercise5
  :: Premise        (p , Not p)
                 --------------------
  -> Conclusion       Bottom

exercise5 = undefined

exercise6
  :: Premise       ( p :\/: (Not p) , p :=>: q, Not p :=>: q)
                 ---------------------------------------------
  -> Conclusion                        q

exercise6 = undefined


class Decidable p where
  lem :: () -> p :\/: (Not p)

derive :: Proposition p -> p
derive (Proof p) = p

exercise7
  :: Decidable p
  => Conclusion  (( p :=>: q ) :<=>: ((Not p) :\/: q))

exercise7 =
    case lem() of
      Left p ->
        Proof ( \f -> Right $ f p
              , \np_q ->
                   case np_q of
                     Left np -> derive $ example3 $ exercise5 $ Proof (p, np)
                     Right q -> const q
              )
      Right np ->
        Proof (\_ -> Left np
              ,\_ -> \p -> derive $ example3 $ exercise5 $ Proof (p, np)
              )
