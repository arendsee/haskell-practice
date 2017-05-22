data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

showTree :: (Show a) => Tree a -> String
showTree Nil = ""
showTree (Node Nil c Nil) = show c
showTree (Node l c r) = show c ++ "(" ++ showTree l ++ "," ++ showTree r ++ ")"


-- this is a terrible algorithm
--  * is slow (requires calling `length` at every iteration)
--  * cannot take infinite input, again, since `length` is called
--  * is not sorted
--  * is not balanced
treeFromList :: [a] -> Tree a
treeFromList []  = Nil
treeFromList [x] = Node Nil x Nil
treeFromList xs  = Node (treeFromList l) c (treeFromList r)
    where
        center' = div (length xs) 2
        l = take center' xs
        c = xs !! center'
        r = drop (center' + 1) xs

-- flatten a tree while preserving leaf order
listFromTree :: Tree a -> [a]
listFromTree Nil = []
listFromTree (Node l c r) = listFromTree l ++ [c] ++ listFromTree r

-- reverse tree
reverseTree :: Tree a -> Tree a
reverseTree Nil = Nil
reverseTree (Node l c r) = Node (reverseTree r) c (reverseTree l)

-- rotate
rotateTree :: Tree a -> Tree a
rotateTree Nil = Nil
rotateTree (Node Nil c r) = Node Nil c r
rotateTree (Node (Node ll lc lr) c r) = Node ll lc (Node lr c r)

-- calculate the depth of the tree
depth :: Tree a -> Int
depth Nil = 0
depth (Node l c r) = 1 + max (depth l) (depth r)

-- apply a function to every element on the tree
treeMap :: (a -> a) -> Tree a -> Tree a
treeMap f Nil = Nil
treeMap f (Node l c r) = Node (treeMap f l) (f c) (treeMap f r)

-- prune tree, cut branches where node value does not meet a condition
pruneTree :: (a -> Bool) -> Tree a -> Tree a
pruneTree f Nil = Nil
pruneTree f (Node l c r)
    | f c       = Node (pruneTree f l) c (pruneTree f r)
    | otherwise = Nil

-- problem, this is really slow
-- also, it is unbalanced
addElement :: (Ord a) => Tree a -> a -> Tree a
addElement Nil x = Node Nil x Nil
addElement (Node l c r) x
    | x < c     = Node (addElement l x) c r
    | otherwise = Node l c (addElement r x)

sortedBinaryTree :: (Ord a) => [a] -> Tree a
sortedBinaryTree = foldl addElement Nil

main = do
    let five' = treeFromList [1..5]
    let ten'  = treeFromList [1..10]

    print $ showTree   five'
    print $ showTree $ reverseTree     five'
    print $ showTree $ rotateTree      five'
    print $ showTree $ treeMap id      ten'
    print $ showTree $ treeMap (^2)    ten'
    print $ showTree $ pruneTree (/=9) ten'

    print $ depth    $ treeFromList [1..10]
    print $ depth    $ treeFromList [1..1000]
    print $ depth    $ treeFromList [1..100000]

    print $ listFromTree $ sortedBinaryTree [4,7,2,45,1,6,0,1,1]

    print $ depth $ sortedBinaryTree [1..1000]

    -- Testing empty list, it erks me that I have to typecast the the empty
    -- list. If I don't ghc complains that it can determine whether a is in
    -- typeclass Show.
    print $ showTree $ treeFromList ([]::[Int])
