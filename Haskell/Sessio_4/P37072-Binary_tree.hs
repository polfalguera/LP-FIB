data Tree a = Node a (Tree a)(Tree a) | Empty
  deriving(Show)

-- Arbres creats per al joc de proves --

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2

size :: Tree a -> Int
size Empty = 0
size (Node _ fe fd) = 1 + (size fe) + (size fd)

height :: Tree a -> Int
height Empty = 0
height (Node _ fe fd) = 1 + max (height fe) (height fd)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node x fxd fxe) (Node y fyd fye)
    | x == y = (equal fxd fyd) && (equal fxe fye)
    | otherwise = False 

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node x fxe fxd) (Node y fye fyd) = (x == y) && ((isomorphic fxe fye && isomorphic fxd fyd) || (isomorphic fxe fyd && isomorphic fxd fye))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node p fe fd) = p : (preOrder fe) ++ (preOrder fd)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node p fe fd) = (postOrder fe) ++ (postOrder fd) ++ [p]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node p fe fd) = (inOrder fe) ++ p : (inOrder fd)

breadthFirst :: Tree a -> [a]
breadthFirst a = breadthFirst' [a] 

breadthFirst' :: [Tree a] -> [a]
breadthFirst' [] = [] 
breadthFirst' (Empty:ts) = breadthFirst' ts 
breadthFirst' ((Node p fe fd):ts) = p : breadthFirst' (ts ++ [fe,fd])

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:xs) ys = (Node x l r)
    where
      l = build x1s y1s
      r = build x2s y2s
      (y1s,y2s) = split x ys
      (x1s,x2s) = splitAt (length y1s) xs

split :: Eq a => a -> [a] -> ([a],[a])
split x [] = ([],[])
split x l = ((takeWhile (/= x) l), tail (dropWhile (/= x) l))

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ Empty t2 = t2
overlap _ t1 Empty = t1
overlap op (Node p1 fe1 fd1) (Node p2 fe2 fd2) = (Node (p1 `op` p2) (overlap op fe1 fe2) (overlap op fd1 fd2))