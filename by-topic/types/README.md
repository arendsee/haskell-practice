In data declarations:

```
data MyThings = Thing1 a b | Thing2 a b c
```

`MyThings` is a type constructor that will be used exactly once, right here,
where the type is declared. `Thing1` and `Thing2` are the data constructors,
that will be used to create to things.
