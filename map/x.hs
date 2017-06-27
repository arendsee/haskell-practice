{-# LANGUAGE ScopedTypeVariables #-}

import Data.Map (Map)
import qualified Data.Map as DM

foo :: [(Int,Int,Int)]
foo = [(1,2,3),(1,3,4),(2,5,6)]

-- The basic goal of this function is to fold a functor into a map. Where
-- elements are merged in some non-monoidal fashion way.

toMap
  :: (Foldable f, Ord k)
  => (a -> Maybe k)      -- try to extract key
  -> (a -> Maybe b)      -- create new element if possible (key not in map)
  -> (a -> b -> Maybe b) -- merge element (key is in map)
  -> Map k b             -- the initial map (likely empty)
  -> f a                 -- the functor of stuff
  -> Map k b             -- the output map

toMap ek mab mabb m0 xs = foldr magic m0 xs
  where
    -- magic :: a -> Map k b -> Map k b
    -- if no key is found, do nothing
    -- otherwise replace current map with result of 'alterB'
    magic a' b' = maybe b' alterB (ek a')
      where

        -- toB :: k -> m b
        -- if the key isn't in the map, do `mab a'``, else 
        -- produce a new map from `mabb a' v`.
        toB k = maybe (mab a') (mabb a') (DM.lookup k b')

        -- alterB :: k -> Map k b
        -- replace the old value in map with the new value,
        -- if no new value was created, do nothing
        alterB k = maybe b' (\v -> DM.insert k v b') (toB k)



main :: IO ()
main = do
  print foo

  let foo2 = toMap (\(x,_,_) -> Just x)
                   (\(_,y,z) -> Just ([y],[z]))
                   (\(_,y,z) (ys,zs) -> Just (ys ++ [y], zs ++ [z]))
                   DM.empty
                   foo

  print foo2



-- toMap
--   :: (Foldable f, Ord k, Monad m)
--   => (a -> m (Maybe k))      -- try to extract key
--   -> (a -> m (Maybe b))      -- create new element if possible (key wasn't in map)
--   -> (a -> b -> m (Maybe b)) -- merge element (key is in map)
--   -> f a                     -- the functor of stuff
--   -> m (Map k b)             -- the output map
--
-- toMap ek mab mabb xs = foldr magic (return DM.empty) xs
--   where
--     -- magic :: a -> m (Map k b) -> m (Map k b)
--     magic a' b' = ek a' >>= maybe b' alterB
--       where
--         -- alterB :: k -> m (Map k b)
--         alterB k = toB k >>= maybe b' (\v -> DM.insert k v <$> b')
--         -- toB :: k -> m b
--         toB k = (DM.lookup k <$> b') >>= maybe (mab a') (mabb a')
