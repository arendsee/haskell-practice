# Functor

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>) = fmap
```

A functor is a thing for which `fmap` is defined. `fmap` changes a value with
out altering the context.

Another function in the Functor typeclass is

```
(<$) :: a -> f b -> f a
(<$) = fmap . const
```

Where `const` is a function in Prelude with the signature

```
const :: a -> b -> a    
```

`(<$)` allows direct replacement of `a` in `f a` with `b`, skipping any
function application.

Functor instances should also follow the functor laws:

```
fmap id = id                          -- first law
fmap (g . h) = (fmap g) . (fmap h)    -- second law
```

The second law follows logically from the first. The first requires the
functor makes no changes to the structure beyond those acting on a single
element. The latter requires that applying g and h to each element is the
same as applying h and the g to all elements. The only case where this
would not be true is when h makes structural changes to the input, which
is illegal by the first law.

Here is an example (from Typeclassopedia) of a Functor that type checks but
breaks the functor laws:

```
-- Evil Functor instance
instance Functor [] where
  fmap _ [] = []
  fmap g (x:xs) = g x : g x : fmap g xs
```

This is improper because fmap implementation alters the structure of the
container (by duplicating each element).

Since `fmap :: (a -> b) -> (f a -> f b)`, fmap can be thought of as a function
that lifts `a -> b` into a mapping in the context of `f`.

I should perhaps read
[the wikibook on category theory](https://en.wikibooks.org/wiki/Haskell/Category_theory)

