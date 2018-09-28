# Good Practice

This is a small collection of best practices for Haskell.

 * Avoid `String`
 * Instances of `Show` should always follow the identity: `read . show == id`.
 * Use [wl-pprint-text](https://hackage.haskell.org/package/wl-pprint-text-1.2.0.0)
