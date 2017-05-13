import Data.List
import Control.Monad

data Tree a = Nil | Node { value :: a, inputs :: [Tree a] } deriving(Show, Eq)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)

instance Foldable Tree where
  foldr _ z Nil = z
  foldr f z (Node a []) = f a z
  foldr f z (Node a (x:xs)) = foldr f (foldr f z x) (Node a xs)

-- Tree to list, just a list of all a
tree2list :: Eq a => Tree a -> [a]
tree2list Nil = []
tree2list (Node x xs) = union [x] (xs >>= tree2list)

-- modify parent by comparing to children
familyMap :: (a -> [a] -> b) -> Tree a -> Tree b
familyMap _ Nil = Nil
familyMap f (Node t ts) = Node new_val new_kids  where
  new_val = f t $ liftM value $ ts
  new_kids = map (familyMap f) ts

-- modify parents based only on children
childMap :: ([a] -> b) -> Tree a -> Tree b
childMap _ Nil = Nil
childMap f (Node _ ts) = Node new_val new_kids where
  new_val = f $ liftM value $ ts
  new_kids = map (childMap f) ts

main :: IO ()
main = do
  let a = Node "jes"   []
  let b = Node "jak"   []
  let c = Node "jen"   [a,b]
  let d = Node "gracy" [c]
  let e = Node "dave"  [d,b]

  print $ tree2list e
  putStrLn "-------------"

  putStrLn "------ forward "
  print $ fmap id e

  putStrLn "------ reverse"
  print $ fmap reverse e

  putStrLn "--------- no jes, foldable test"
  print $ all (/= "jes") e

  putStrLn "--------- child map"
  print $ childMap length e

  print $ "Total children: " ++ (show . foldr (+) 0 . childMap length) e

  putStrLn "--------- family map"
  print $ tree2list $ familyMap f e
    where
    f :: Show a => a -> [a] -> String
    f p c = (show p) ++ " has " ++ ((show . length) c) ++ " children"
