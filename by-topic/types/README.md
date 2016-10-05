# Type versus Data Constructors

In data declarations:

```
data MyThings = Thing1 a b | Thing2 a b c
```

`MyThings` is a type constructor that will be used exactly once, right here,
where the type is declared. `Thing1` and `Thing2` are the data constructors,
that will be used to create to things.

# Algebraic types

Types that have multiple data constructors are called algebraic types.

The simplest example being the Bool type:

```
data Bool = True | False
```

An algebraic data type is a composite data type. See the wikipedia article. 

Definitions:

    * `Typeclasses` define a set of functions that can have different
      implementations depending on the type of data they are given [1]


[1] Real World Haskell
