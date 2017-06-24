{-# LANGUAGE ScopedTypeVariables #-}

import Data.Map (Map)
import qualified Data.Map as DM

foo :: [(Int,Int,Int)]
foo = [(1,2,3),(1,3,4),(2,5,6)]

-- The basic goal of this function is to fold a functor into a map. Where
-- elements are merged in some non-monoidal fashion way.

toMap ::
  (
    Foldable f
  , Ord k
  )
    => (a -> Maybe k)    -- try to extract key
    -> (a -> b -> b)     -- merge element (key is in map)
    -> (a -> b)          -- create new element (key wasn't in map)
    -> Map k b           -- the initial map (likely empty)
    -> f a               -- the functor of stuff
    -> Map k b           -- the output map

toMap ek mabb mab m0 xs = foldr magic m0 xs where
  magic x m =
    case ek x of
      Nothing -> m
      Just k -> case DM.lookup k m of
        Nothing -> DM.insert k (mab x) m
        Just y -> DM.insert k (mabb x y) m

main :: IO ()
main = do
  print foo

  let foo2 = toMap (\(x,_,_) -> Just x)
                   (\(_,y,z) (ys,zs) -> (ys ++ [y], zs ++ [z]))
                   (\(_,y,z) -> ([y],[z]))
                   DM.empty
                   foo

  print foo2
