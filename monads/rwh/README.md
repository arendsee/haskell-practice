Monad rules:

 1. `m a`
 2. `m a -> (a -> m b) -> m b
 3. `a -> m a`

Terms (adapted from RWH):

 * "Monadic" - "pertaining to monads". A monadic type is an instance of the
   Monad typeclass; a monadic value has a monadic type.

 * "is a monad", shorthand for being an instance of the Monad typeclass. Must
   have the monadic triple: type constructor, injection function, and
   chaining function.

 * "the Foo monad" - implies Foo is an instance of the Monad typeclass.

 * "action" another name for a monadic value.

The pure function is wrapped in the impure monad. Lift functions lift the pure
entity from its impure trappings, pass it through a pure function, then rewrap
it.


A decomposition of bind (from Eric Kidd's comment in RWH)

(>>=) :: m a -> (a -> m b) -> m b

Can be expressed as a composition of

join :: m ( m a ) -> m a
fmap :: (a -> b) -> (m a -> m b)

So you have:

f :: a -> m b
fmap f :: m a -> m ( m b )
join . fmap f :: ma -> mb
