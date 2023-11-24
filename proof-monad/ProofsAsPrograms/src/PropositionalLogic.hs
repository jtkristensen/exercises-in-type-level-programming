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
newtype Proposition program = Proof { derive :: program }

type Premise    = Proposition
type Conclusion = Proposition

instance Functor Proposition where
  fmap f (Proof a) = Proof (f a)

instance Applicative Proposition where
  pure    = Proof
  f <*> x = Proof $ derive f $ derive x

instance Monad Proposition where
  return   = pure
  ma >>= f = f (derive ma)

-- Examples

example1
  ::      Premise a  ->  Premise b
  ----------------------------------------
  ->       Conclusion (a :/\: b)

example1 = (<*>) . fmap (,)

example2
  ::      Premise (a :=>: b) -> Premise a
  ----------------------------------------
  ->               Conclusion b

example2 = (<*>)

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

exercise1 fg =
  do (f, g) <- fg
     return $ g . f

exercise2
  :: Premise       ((a :\/: b) , (a :=>: c) , (b :=>: c))
                 ------------------------------------------------
  -> Conclusion                         c

exercise2 premise =
  do (aOrB, f, g) <- premise
     return $
       case aOrB of
         Left  a -> f a
         Right b -> g b

exercise3
  :: Premise          ((a :/\: b) :\/: c)
                  ------------------------------
  -> Conclusion    ((a :\/: c) :/\: (b :\/: c))

exercise3 =
  fmap $ either (\(a, b) -> (Left a, Left b)) (\c -> (Right c, Right c))

exercise4
  :: Premise          ((a :\/: b) :/\: c)
                  ------------------------------
  -> Conclusion    ((a :/\: c) :\/: (b :/\: c))

exercise4 premise =
  do (aOrB, c) <- premise
     return $ either (Left . flip (,) c) (Right . flip (,) c) aOrB

exercise5
  :: Premise        (p , Not p)
                 --------------------
  -> Conclusion       Bottom

exercise5 pNp =
  do (p, np) <- pNp
     return $ np p

exercise6
  :: Premise       ( p :\/: (Not p) , p :=>: q, Not p :=>: q)
                 ---------------------------------------------
  -> Conclusion                        q

exercise6 premise =
  do (pNp, f, g) <- premise
     return $ either f g pNp

-- Harder exercise

class Decidable p where
  lem :: Proposition (p :\/: (Not p))

exercise7
  :: Decidable p
  => Conclusion  (( p :=>: q ) :<=>: ((Not p) :\/: q))

exercise7 =
  do pOrNp <- lem
     return $
       case pOrNp of
         Left   p -> (Right . ($ p), either ((.) $ derive . example3 . Proof) const)
         Right np -> (const $ Left np , const $ derive . example3 . Proof . np)
