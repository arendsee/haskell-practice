Monads are:

 * a construct in category theory [1]


```
    A monad is constructed on top of a polymorphic type such as IO. The monad
    itself is defined by instance declarations associating the type with the some
    or all of the monadic classes, Functor, Monad, and MonadPlus. None of the
    monadic classes are derivable. In addition to IO, two other types in the
    Prelude are members of the monadic classes: lists ([]) and Maybe. [1]
```

Monads are governed by a set of laws, understand the laws and you will understand monads (Maybe).

``` haskell
-- The monad class defines two operatores: bind (>>=) and return

infixl 1  >>, >>=                                -- 1
class  Monad m  where                            -- 2
    (>>=)            :: m a -> (a -> m b) -> m b -- 3
    (>>)             :: m a -> m b -> m b        -- 4
    return           :: a -> m a                 -- 5
    fail             :: String -> m a            -- 6
    m >> k           =  m >>= \_ -> k            -- 7
```
[1]


``` haskell
-- The do syntax provides a simple shorthand for chains of monadic operations. The
-- essential translation of do is captured in the following two rules:

  do e1 ; e2      =  e1 >> e2         -- 8
  do p <- e1; e2  =  e1 >>= \p -> e2  -- 9
```
[1]

When using `>>` you just move on to thw next value, with no tracing of the past.

When using `>>=` you carry you state with you.

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

References:

 1. https://www.haskell.org/tutorial/monads.html
 2. Wadler, Philip. 1995. Monads for functional programming


@inproceedings{wadler1995monads,
  title={Monads for functional programming},
  author={Wadler, Philip},
  booktitle={International School on Advanced Functional Programming},
  pages={24--52},
  year={1995},
  organization={Springer}
}
