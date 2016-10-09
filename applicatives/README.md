A functor is a typeclass that requires definition of fmap

```
class Functor a where
    fmap :: (a -> b) -> f a -> f b
```

This is exactly the functionallity I happened upon in smof, where I defined
transforms for all my data types.

`fmap` allow cool things like:

```
fmap (+1) (Just 1)
fmap (+2) [1,2,3]
map (fmap (+2)) [Just 1, Nothing, Just 2]
```

Applicatives allow both value *and* function to be wrapped in context.

```
import Control.Applicative

[(*2), (*5)] <*> [1,2,3]
Nothing <*> Nothing  -- and nothing ever will
Just reverse <*> Just [Just 1, Nothing, Just 3]
(*) <$> Just 5 <*> Just 3
```



Monads

A monad is a type class where bind and return are defined.

A monad is a datatype that is an instance of this type class.

A monad is the `m a` in `(>>=) :: m a -> (a -> m a) -> m a`.

A monad is a Functor which is an Applicative

```
half x
    | even x    = Just (div x 2)
    | otherwise = Nothing
Just 24 >>= half >>= half
```


I could define the Functor and Applicative in the same way as bind
```
(>>$) :: fa -> (a -> b) -> f b
(>>$) = flip (<$>)

(>>*) :: f a -> f (a -> b) -> f b
(>>*) :: flip (<*>)
```

With these redefinitions, the three can easily be compared:
    * Functor:     `(>>$) :: f a ->   (a ->   b) -> f b`
    * Applicativs: `(>>*) :: f a -> f (a ->   b) -> f b`
    * Monad:       `(>>=) :: f a ->   (a -> f b) -> f b`

```
getLine >>= readFile >>= putStrLn
```

Holy Monad!!!

References

[adit.io](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
