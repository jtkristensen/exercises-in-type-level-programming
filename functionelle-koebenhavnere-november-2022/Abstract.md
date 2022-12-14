

## Dependent types modulo phase distinction.

At Funktionelle Koebenhavnere, we have discussed type-level programming on
several occations. Notably, in Joakim Ahnfelt-Rønne's talk about using
`GHC.Generics` to implement `DerivingFunctor` in Haskell.

Other language extensions for Haskell such as `GADTs` and `TypeFamilies`
allows us to promote run-time values to the type level. Thus making Haskell
a dependently typed language (in a certain sense).

In this meeting, we will discuss what computations we might want to be able
to express at the type level, and see how to do it in Haskell, and solve a
couple of exerises `{~_^}`.
