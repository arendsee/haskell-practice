data Tree a = Nil | Node {value :: a, inputs :: [Tree a]} deriving(Show)

zip' :: (a -> a) -> a -> Tree b -> Tree (a,b)
zip' _ _  Nil = Nil
zip' f x (Node y kids) = Node (x,y) (mapzip' f (f x) kids) where
  mapzip' :: (a -> a) -> a -> [Tree b] -> [Tree (a,b)]
  mapzip' _ _ [] = []
  mapzip' f' x' (t:ts) = [top] ++ mapzip' f' (fst . value $ top) ts where
    top = zip' f' x' t

main :: IO ()
main = do
  let a = Node "jes"   []
  let b = Node "jak"   []
  let c = Node "jen"   [a,b]
  let d = Node "gracy" [c]
  let e = Node "dave"  [d,b]

  print e
  print $ zip' (+ 1) 1 e
