{-# 
   LANGUAGE
      MultiParamTypeClasses
      , TypeSynonymInstances
      , FlexibleInstances
      , OverlappingInstances
      , IncoherentInstances
#-}

module Foo (Foo(..)) where

class Foo a b where
  foo :: a -> b

-- | Default value
instance Foo a String where
  foo = const "yolo"

-- | Specifc value
instance Foo Int String where
  foo = show

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
