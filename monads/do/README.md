# Do notation

I've found this confusing in the past. Since it is close to imperative
programming, I haven't had too much trouble using it. But I miss the nuance and
the monadic relation. What I was missing was a formal set of rules describing
what the notation does.

This description, from Stephen Diehls (btw, you are boss and I am in your
debt). Describes the relation cleanly.

```
do { a <- f ; m }  ===  f >>= \a -> do { m }  -- bind 'f' to a, desugar 'm'

do { f ; m }  ===  f >> do { m }              -- evaluate 'f', then desugar  m

do { m }  ===  m
```

The recursive definition is absolutely clear, a simple algorithm.
