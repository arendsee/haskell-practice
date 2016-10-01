data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

showTree :: (Show a) => Tree a -> String
showTree Nil = ""
showTree (Node Nil c Nil) = show c
showTree (Node l c r) = (show c) ++ "(" ++ (showTree l) ++ "," ++ (showTree r) ++ ")"


treeFromList :: [a] -> Tree a
treeFromList []     = Nil
treeFromList (x:[]) = Node Nil x Nil
treeFromList xs     = Node (treeFromList l) c (treeFromList r)
    where
        center' = div (length xs) 2
        l = take center' xs
        c = xs !! center'
        r = drop (center' + 1) xs
             

main = do
    print $ showTree $ treeFromList [1]
    print $ showTree $ treeFromList [1,2]

    -- Testing empty list, it erks me that I have to typecast the the empty
    -- list. If I don't ghc complains that it can determine whether a is in
    -- typeclass Show.
    print $ showTree $ treeFromList ([]::[Int])
