import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs)) (treeFromList (filter (>x) xs))

inOrderFlatten Empty = []
inOrderFlatten (Node x left right) = inOrderFlatten left ++ [x] ++ inOrderFlatten right

preOrderFlatten Empty = []
preOrderFlatten (Node x left right) = [x] ++ preOrderFlatten left ++ preOrderFlatten right

postOrderFlatten Empty = []
postOrderFlatten (Node x left right) = postOrderFlatten left ++ postOrderFlatten right ++ [x]