# Polytyped functions

I want to take a heterofunctor and convert it to a homofunctor. Let `E` be
a type that varies across elements in the functor and `a` be a normal generic
variable. Then I want to do:

```
(E -> b) -> f (a -> E) -> f (a -> b)
```

This general form is an `fmap`. The problem is the function `(E -> b)`, if
E can really be anything, this function is impossible, except for the trivial
case where `b` is bottom. However, if there is some constraint on `E`,
something all `E` have in common, then nontrivial `(E -> b)` functions may
exist. For example, we may constrain types in `E` to be members of the Show
typeclass, ensuring they all have a `show` function that converts them to
strings. Then we could do

We can get this behaviour with existential types. Where we define E to the set
of all types that follow some constraint.

```
data E = forall a. Show a => E a
```

Then we can map away
