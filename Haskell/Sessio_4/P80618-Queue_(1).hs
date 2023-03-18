data Queue a = Queue [a] [a]
     deriving (Show)

c = push 3 (push 2 (push 1 create))

{- h=> head, t=> tail -}

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue h t) = (Queue h (x:t))

pop :: Queue a -> Queue a
pop (Queue [] []) = (Queue [] [])
pop (Queue (x:h) t) = (Queue h t)
pop (Queue h t) = (Queue new_h [])
    where
        new_h = tail $ (reverse t)

top :: Queue a -> a
top (Queue (x:h) _) = x
top (Queue _ t) = takeLast t 
    where
        takeLast :: [a] -> a
        takeLast [x] = x
        takeLast (x:xs) = takeLast xs 

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _)   = False 

c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
c2 = push 4 (push 3 (push 2 create))

instance Eq a => Eq (Queue a)
     where
        (Queue h1 t1) == (Queue h2 t2) = ((h1 ++ (reverse t1)) == (h2 ++ (reverse t2)))