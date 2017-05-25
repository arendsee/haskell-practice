# Monads

Monads are instances of a typeclass that defines a `return` and `bind` function
where 3 laws must be followed.

``` haskell
infixl 1  >>, >>=                        -- 1
class  Monad m  where                    -- 2
    return   :: a -> m a                 -- 3
    (>>=)    :: m a -> (a -> m b) -> m b -- 4
    (>>)     :: m a -> m b -> m b        -- 5
    fail     :: String -> m a            -- 6
    m >> k   =  m >>= \_ -> k            -- 7
```

Only the first two functions are required.


Here are the three laws, stated as lists of equalities

```
-- law 1 ------------------------------------------------------------
return x >>= f

do
  y <- return x
  f y

f x


-- law 2 ------------------------------------------------------------
do
  x <- f
  return x

do
    f a

f a >>= return


-- law 3 ------------------------------------------------------------
do
    y <- do
        x <- f
        g x
    h y

do
    x <- f
    y <- g
    h y

do
    x <- f
    do
        y <- g
        h y
```

Law 3 is basically an associative law, which is pretty plain in the Do
notation, but a little hard to see otherwise. It would be fun to try to find
examples of valid instances of typeclass Monad (where `return` and `bind` are
defined), that violate each of these laws.


``` haskell
-- The do syntax provides a simple shorthand for chains of monadic operations. The
-- essential translation of do is captured in the following two rules:

  do { e1 ; e2     }  =  e1 >> e2         -- 8
  do { p <- e1; e2 }  =  e1 >>= \p -> e2  -- 9
```
[1]

When using `>>` you just move on to thw next value, with no tracing of the past.

When using `>>=` you carry state with you.

In <9> the attachment of p to e1 needs to be transferred to the next do line.

Monads provide modularity. See [2].

"monads ... integrate impure effects into pure functional languages" [2]

"a pure functional language is written as a set of equations" [2]

Actually, [2] is pure gold. Not just of relevance to monads, but to
functional programming in general. Passing state into a purely functional
languages (PFL) can lead to parameter bloat. In a PFL you cannot modify
the environment. If you want to return a error message from a recursive
call, you have to pass the message as a parameter. If you want to track
the depth of a call, you have to pass a count parameter. Modifying
existing code to do this breaks modularity. Enter the monad.

Following Brian Beckman [3]:

1. Functions

Instead of `int x`, `x : int`. Now functions are `f : int -> int`. Now make
generic, `f : a -> a`, where `a` is any type.

2. Monoids

Monoids are easy, just type where an associative operator exists that can
produce a new element in the set from any two elements in the set. Like
counting numbers upon addition or multiplication. Like lists, where you can
concatentate two lists into one. Like graphs, where you can take the union of
edges.

```
f : a -> a
g : a -> a
-- f(g a)
h = f . g
h :: a -> a
```

h is a monoid, a combination of two functions which have the same type.

This is **the** way to build complexity.

Monoid - a set of things plus a set of rules for combining the things, that follow a set of rules

A mapping rule plus a type of data.

Rule 1: Monoids are associative: `(a . b) . c = a . (b . c)`
Rule 2: Identity, i exists such that ` a . i = a`

Does NOT require commutativity.

See clock example

3. Functions

Functions under composition are monoids

4. Monads

Let's talk about functions that return transforms of input

```
f' :: a -> M a
g' :: a -> M a
```

 * Monads are used to deal with complexity

 * Monads allow composition

 * The bind operator is a composition operator for composing f' and g'

 * Once you have monads, you are magic, you can prove stuff, recombine in any way

 * Once you have a set of monads, you can combine them in any way

Another word - Monoid Category

Now on to category theory [4]

Objects - sets
Morphisms - functions on sets - closed on composition

Mostly category theory is a system of syntax.

Types are spaces and spaces are types

Let's say we have a category of categories
Objects are categories
Morphisms are functors, maps from one category to another
Functor - a map between category that preserves structure
Natural transormations - maps between functors

Might also be useful to look up commutative diagrams, see wikipedia [5]

In case I thought monads were hard ... Yoneda lemma.

References:

 1. https://www.haskell.org/tutorial/monads.html
 2. Wadler, Philip. 1995. Monads for functional programming
 3. Brian Beckman. https://www.youtube.com/watch?v=ZhuHCtR3xq8
 4. Tom LaGatta. https://www.youtube.com/watch?v=o6L6XeNdd\_k
 5. https://en.wikipedia.org/wiki/Commutative\_diagram
 6. [All About Monads](https://wiki.haskell.org/index.php?title=All_About_Monads&oldid=54386)

@inproceedings{wadler1995monads,
  title={Monads for functional programming},
  author={Wadler, Philip},
  booktitle={International School on Advanced Functional Programming},
  pages={24--52},
  year={1995},
  organization={Springer}
}
