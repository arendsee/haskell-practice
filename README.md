# Haskell Practice

I have been planning on learning a functional language for some time, but there
was never a sufficiently strong reason for me to do so. But recently,
I converted from Gnome desktop to Xmonad, a Haskell-based window manager. The
configuration files are all in Haskell. I don't understand them, but I really,
really want to remap my workspace keys. So ... learn Haskell.

# Sources of information

These are the primary sources. Both are cleanly written and comprehensive.

 1. GHC manual - GHC details (e.g. optimizations) and info on all extensions
 2. Haskell 2010 language report - everything about the language
    [report](https://www.haskell.org/onlinereport/haskell2010/)

# Things I naively think are true

 1. all variables are immutable
 2. functions take precedence over everything
 3. variables needn't be declared before textual use, only before evaluation
 4. '=' is not assignment, but equivalence, like in math
 5. 'where' is used for local local values (as opposed to values local to
    a module that are not imported)
 6. `do` is a special syntax for monads

# Contents

 1. by-topic - material loosely following a tutorial
 2. scratch - random stuff, some modified from tutorials
 3. screw it, I'll never keep this up-to-date

# Terminology

 1. *Partial* versus *total* functions - partial functions are not defined across
    their entire domain, for example `head`, which dies on empty lists.
