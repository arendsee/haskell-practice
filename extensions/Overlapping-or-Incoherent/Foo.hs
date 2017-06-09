{-# 
   LANGUAGE
      MultiParamTypeClasses
      , TypeSynonymInstances
      , FlexibleInstances
      , IncoherentInstances
#-}

module Foo (Foo(..)) where

class Foo a b where
  foo :: a -> b

-- | Default value
instance Foo a String where
  foo = const "yolo"

-- | Specifc value
instance {-# Overlapping #-} Foo String String where
  foo = show

instance {-# Overlapping #-} Foo Bool Int where
  foo True  = 1
  foo False = 0

-- All the above now works. But I want to do this:
--
-- instance Foo a String where
--   foo = const "yolo"
--
-- instance (Show a) => Foo a String where
--   foo = show

-- This seems reasonable, at least in this case, I can figure out the right
-- thing unambiguously. Use `show` if the `a` is a member of Show, else use
-- `const "yolo"`. However, this would become ambiguous if the constraints
-- overlap.  But is their an extension that allows this anyway, calling an
-- error only in ambiguous cases?
--
-- This doesn't seem to be possible, currently.
