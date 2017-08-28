# Language extensions

Here I will describe various extensions of the Haskell language. Each folder
will describe one extension along with usage examples.

Caveate, many extensions tend to be used together, so one folder might contain
multiple extensions. I'll shamelessly concatenate extensions names in the
folder name.


## FlexibleContexts

   https://prime.haskell.org/wiki/FlexibleContexts

   Allows concrete types, rather than only type variables, in function constraints.

   The use is explained well [here](https://stackoverflow.com/questions/31251163).

   Most usefule with `MultiParamTypeClasses`

## MultiParamTypeClasses

   Allows type classes with multiple parameters

## TypeSynonymInstances

    Here is the brief from https://prime.haskell.org/wiki/TypeSynonymInstances:

```
    Haskell 98 permits only type constructors defined using data or newtype in
    instance heads.

    The proposal is to allow type synonyms (fully applied, as ever) in instance
    heads. These would be fully expanded before any other restrictions on
    instance heads were checked (see FlexibleInstances and
    OverlappingInstances).

    Not very useful without either FlexibleInstances or UndecidableInstances.
```

    The rational for the last sentence is that constructors aren't legal anyway in contexts without FlexibleInstances.

## FlexibleInstances

   Allow constructors in instances

## UndecidableInstances

## TypeFamilies

## Rank2Types

## ConstraintKinds
